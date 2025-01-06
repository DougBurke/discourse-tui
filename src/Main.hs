{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Brick.Widgets.List as WL

import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V

import qualified System.Info as SI

-- import qualified PackageInfo_discourse_tui as P
import qualified Paths_discourse_tui as P

import qualified Formatting as F
import qualified Formatting.Time as FT

import Brick (BrickEvent(..), App(..), EventM, Padding(..),
              ViewportType(Both),
              HScrollBarOrientation(OnBottom),
              VScrollBarOrientation(OnRight),
              Widget,
              Direction(..),
              nestEventM', attrName, get, put,
              (<=>), (<+>),
              attrMap, defaultMain,
              fg,
              hBox, hLimit,
              neverShowCursor,
              padBottom, padLeft, padRight, padTop,
              txt, txtWrap,
              viewport, viewportScroll,
              vScrollBy, vScrollToBeginning, vScrollToEnd, vScrollPage,
              hScrollBy,
              emptyWidget,
              withHScrollBars, withVScrollBars,
              withAttr)
import Brick.Main (halt)
import Brick.Widgets.Border (hBorder, hBorderAttr)
import Brick.Widgets.Center (hCenter)

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (isEmptyMVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Exception (IOException, catch, evaluate)
import Control.Lens (Getting
                    , (&), (.~), (^.), _2
                    , view)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (bimap)
import Data.List (intercalate, foldl')
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Version (showVersion)

import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(MShift))
import Graphics.Vty.Attributes (blue, bold, defAttr, dim, green, reverseVideo, withStyle, cyan, yellow)

import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest, setRequestQueryString)

import System.Directory (XdgDirectory(XdgConfig), getXdgDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (die, exitFailure)
import System.Process (spawnProcess)
import Text.Pandoc (runIO, def, handleError, readHtml, writeCommonMark)

import Types

-- Change a protoTopic into a topic by consulting userMap and catagoryMap.
-- Unfortunately we can not guarantee that all categories are recorded in
-- the categoryMap (at least the Haskell discourse doesn't seem to
-- return all items). Actually, maybe it's the "subcategory" support in
-- the categories endpoint which is less-than-ideal.
--
parseTopic :: M.IntMap User -> M.IntMap Category -> ProtoTopic -> Topic
parseTopic userMap catagoryMap pt
    = Topic {
        _title = pt ^. pTitle,
        _topicId = pt ^. pTopicId,
        _category = (catagoryMap M.! (pt ^. pCategoryId)) ^. categoryName,
        _lastUpdated = pt ^. pLastUpdated,
        _likeCount = pt ^. pLikeCount,
        _postsCount = pt ^. pPostsCount,
        _posters = V.map (\x -> (userMap M.! (x ^. posterId)) ^. userName) (pt ^. pPosters),
        _pinned = pt ^. pPinned
            }

helpMessage :: Aliases -> String
helpMessage (Aliases aliases) =
  let alines = if null aliases
               then []
               else [ "Available aliases:"
                    , ""
                    ] <> map convert aliases <> [""]

      convert (k, v) = "  " <> k <> ": " <> v

  in intercalate "\n"
     ([ "Usage:   discourse-tui url|fragment"
      , "options: --help | --version"
      , "version: " <> showVersion P.version
      , ""
      , "where fragment (no . character) is taken to mean https://discourse.fragment.org,"
      , "or the full URL is given. So either:"
      , ""
      , "     discourse-tui haskell"
      , "     discourse-tui https://discourse.haskell.org"
      , ""
      , "Aliases can be stored in a file called aliases in the directory"
      , "$XDG_CONFIG_HOME/discouse-tui, and the form is"
      , ""
      , "    alias url"
      , ""
      , "with one alias per line and no leading or trailing spaces."
      , ""
      ] <> alines)


-- Get the GHC version info.
--
getVersion :: String
getVersion =
  "version: " <> showVersion P.version <>
  " (" <> SI.compilerName <> " " <>
  showVersion SI.fullCompilerVersion <>
  " " <> SI.os <> " " <> SI.arch <> ")"


reportVersion :: IO ()
reportVersion = do
  name <- getProgName
  putStrLn (name <> ": " <> getVersion)
  exitFailure


-- Not worth a map yet

newtype Aliases = Aliases [(String, String)]
  deriving (Semigroup, Monoid, Show)

-- Read the aliases from the XDG_CONFIG_HOME / "discourse-tui" / "aliases"
-- or return the default (empty list) if not set.
--
aliasPath :: IO String
aliasPath = do
  base <- getXdgDirectory XdgConfig "discourse-tui"
  pure (base <> "/" <> "aliases")


readAliases :: IO Aliases
readAliases = catch _read errHandler
  where
    errHandler :: IOException -> IO Aliases
    errHandler _ = pure mempty

    _read = do
      filename <- aliasPath
      getAliases <$> readFile filename


-- forget the error checking for now
getAliases :: String -> Aliases
getAliases cts =
  let ls = lines cts
      alltoks = map (span (/= ' ')) ls
      cleantoks = map (bimap clean clean) alltoks
      toks = filter (\(l,r) -> "" `notElem` [l,r]) cleantoks

      cleanEdge = dropWhile (== ' ')
      clean x = let a = cleanEdge (reverse x)
                in cleanEdge (reverse a)

  in Aliases toks


-- Expand out the alias if known, otherwise return the key.
-- Name is a bit confusing now I've re-written some things.
--
checkIfAlias :: Aliases -> String -> String
checkIfAlias (Aliases a) k = case lookup k a of
  Just v -> v
  _ -> "https://discourse." <> k <> ".org"


checkArg :: Aliases -> String -> String
checkArg aliases key =
  if '.' `elem` key
  then key
  else checkIfAlias aliases key


parseArgs :: IO String
parseArgs = do
    aliases <- readAliases
    args <- getArgs
    when ("--help" `elem` args) (die (helpMessage aliases))
    when ("--version" `elem` args) reportVersion
    case args of
      ['-':rest] -> die ("Unknown option: -" <> rest)
      [x] -> pure (checkArg aliases x)
      _ -> die (helpMessage aliases)


main :: IO ()
main = do
    baseUrl <- parseArgs
    initialState <- getTuiState baseUrl
    void $ defaultMain tuiApp initialState


-- initialize the TuiState with the list of topics and catagories
getTuiState :: String -> IO TuiState
getTuiState "" = die "The discourse URL is empty"
getTuiState baseUrl = do
  let baseUrl' = if last baseUrl == '/' then baseUrl else baseUrl <> "/"

  topicsRequest <- parseRequest (baseUrl' <> "latest.json")
  categoriesRequest <- parseRequest (baseUrl' <> "categories.json" <> "?include_subcategories=true")
  categoriesResp <- getResponseBody <$> httpJSON categoriesRequest
  tps <- getResponseBody <$> httpJSON topicsRequest

  now <- getCurrentTime
  let cat = makeCategoryMap categoriesResp
  pure (processTopics baseUrl' now cat tps)


tokenize :: Getting M.Key a M.Key -> V.Vector a -> M.IntMap a
tokenize f = M.fromList . V.toList . V.map (\x -> (x ^. f, x))


makeCategoryMap :: CategoryResponse -> (M.IntMap Category, CategoryList)
makeCategoryMap resp =
  let catMap = tokenize categoryId catList <>
               tokenize categoryId subCatList

      catList = resp ^. categories
      cats = WL.list Categories catList 1

      -- Pull out the subcategories *AND* convert them to categories
      -- so that categoryMap works.
      --
      subCatList = V.concatMap (\cat -> V.map convertSubCat (cat ^. subCategoryList)) catList

  in (catMap, cats)


-- this should not return a TuiState
processTopics ::
  String
  -> UTCTime
  -> (M.IntMap Category, CategoryList)
  -> TopicResponse
  -> TuiState
processTopics baseUrl now (catMap, cats) tps =
  let users = tps ^. tpUsers
      userMap = tokenize userId users

      topList = V.map (parseTopic userMap catMap) (tps ^. tpTopicList)
      topics = WL.list Contents topList 3

  in TuiState {
    _currentTime = now
    , _topicList = topics
    , _categoryList = cats
    , _categoryMap = catMap
    , _baseURL = baseUrl
    , _timeOrder = Increasing
    , _displayState = initDisplayState
    }


-- The help bar at the bottom, for most pages.
--
helpBase :: T.Text
helpBase = "h help | q to quit"

withAttrName :: String -> Widget n -> Widget n
withAttrName n = withAttr (attrName n)


helpBar :: UTCTime -> Maybe TimeOrder -> Maybe ExtraDownload -> Widget ResourceName
helpBar _ Nothing _ = withAttrName "bar" (txt helpBase)
helpBar time (Just order) (Just ed) = withAttrName "bar" widget
  where
    widget = txt helpBase <+> downloading <+> dirMsg
    dirMsg = padLeft Max $ txt (showOrder order)
    downloading = hCenter (txt ("downloading: " <>
                                showInt (ed ^. edRunning) <>
                                "/" <>
                                showInt (V.length (ed ^. edToDo)) <>
                                " (" <> showTimeDelta (ed ^. edTime) time <>
                                ")"
                               )
                          )


helpBar _ (Just order) Nothing = withAttrName "bar" widget
  where
    widget = txt helpBase <+> dirMsg
    dirMsg = padLeft Max $ txt (showOrder order)

-- The help bar for the single-post page
helpPostBar :: TimeOrder -> Int -> Int -> Widget ResourceName
helpPostBar order cur nposts = withAttrName "bar" widget
  where
    widget = txt msgBase <+> padLeft Max (txt right)

    right = if nposts == 1
            then ""
            else showInt n <> "/" <> showInt nposts <> " " <> showOrder order

    -- note: cur is 1 based
    n = case order of
          Increasing -> cur
          Decreasing -> nposts - cur + 1

    msgBase = "h help | q to quit"


showOrder :: TimeOrder -> T.Text
showOrder Increasing = "↑" -- unicode 2191  "+"
showOrder Decreasing = "↓" -- unicode 2193  "-"


-- Get the posts for the current topic. The initial query
-- uses t/topicid.json which returns the first set of data and
-- a list of all the posts in the topic. We then set up a
-- thread to download the remaining data, which will be checked
-- by handleTuiEvent.
--
-- It would be nice to only call pandoc if we need it (but
-- we do need it to be able to display the summary in the posts
-- view).
--
getPosts ::
  TuiState
  -> TopicList
  -> IO SingleTopic
getPosts tui tl = do
    let Just selectedTopicID = view (_2 . topicId) <$> WL.listSelectedElement tl
        postURL = mconcat [tui ^. baseURL, "t/", show selectedTopicID, ".json"]
        extraURL = mconcat [tui ^. baseURL, "t/", show selectedTopicID, "/posts.json"]

    postsRequest <- parseRequest postURL
    pr <- getResponseBody <$> httpJSON postsRequest
    basePosts <- mapM postToPandoc (pr ^. postList)

    let allIds = S.fromList . V.toList $ pr ^. postIds
        gotIds = S.fromList . V.toList . V.map (^. postId) $ pr ^. postList
        extraIds = V.fromList . S.toAscList $ allIds `S.difference` gotIds

        nposts = V.length (pr ^. postIds)

    mExtra <- getExtraPosts extraURL extraIds
    pure $ toSingleTopic (pr ^. postResponseId) nposts (pr ^. postSlug) (pr ^. postTitle) (WL.list Posts basePosts 1) mExtra


getExtraPosts :: String -> V.Vector Int -> IO (Maybe ExtraDownload)
getExtraPosts _ ids | V.null ids = pure Nothing
getExtraPosts baseUrl ids = do
  let (now, later) = V.splitAt 20 ids

      getChunk = do
        let qry = map (\x -> ("post_ids[]", toN x)) (V.toList now)
            toN = Just . B.pack . show
        req <- parseRequest baseUrl
        let req' = setRequestQueryString qry req
        rsp <- getResponseBody <$> httpJSON req'
        pure (rsp ^. postSelList)

  mvar <- newEmptyMVar
  stime <- getCurrentTime
  tid <- forkIO $ do
    posts' <- getChunk >>= mapM postToPandoc >>= evaluate
    putMVar mvar posts'

  pure . Just $ toExtraDownload stime baseUrl (V.length now) later mvar tid


postToPandoc :: Post -> IO Post
postToPandoc post = do
    newContents <- toMarkdown $ post ^. contents
    pure $ post & contents .~ newContents


toMarkdown :: T.Text -> IO T.Text
toMarkdown s = do
    result <- runIO $ do
        doc <- readHtml def s
        writeCommonMark def doc
    handleError result

-- Want to query /c/<slug>/<id>.json
--
getCategory ::
  TuiState
  -> IO Display
getCategory tui = do

    -- need a lens to get the pair of values
    let mcat = WL.listSelectedElement (tui ^. categoryList)
        Just idVal = view (_2 . categoryId) <$> mcat
        Just slugVal = view (_2 . categorySlug) <$> mcat
        Just nameVal = view (_2 . categoryName) <$> mcat
        postURL = mconcat [url, "c/", T.unpack slugVal, "/", show idVal, ".json"]

        url = tui ^. baseURL
    
    -- We have the category map so shouldn't need to request the categories again.
    --
    topicsRequest <- parseRequest postURL
    tps <- getResponseBody <$> httpJSON topicsRequest

    let ntps = processTopics url (tui ^. currentTime) (tui ^. categoryMap, tui ^. categoryList) tps ^. topicList
        ndisplay = DisplayCategory slugVal idVal nameVal ntps

    pure ndisplay

-- This reverses the list *BUT* keeps the selected element,
-- which is a bit odd, and up/down still follow the ordering
-- of the original list, which is also odd.
--
-- Changing the selected position should be possible, but it
-- needs to be done when the ordering is switched, and then
-- that is a problem as the order is global but there are
-- two lists it's used with. Aha. I have decided to only
-- apply it to the posts lists, which makes things a bit
-- easier.
--
orderSelect :: TimeOrder -> WL.List a b -> WL.List a b
orderSelect Increasing xs = xs
orderSelect Decreasing xs =
  let mPos = WL.listSelected xs
      out = WL.listReverse xs
  in case mPos of
    Just cPos -> WL.listMoveTo cPos out
    Nothing -> out


tuiApp :: App TuiState e ResourceName
tuiApp =
  let attrs = attrMap defAttr [ (attrName "title", withStyle defAttr bold)
                              , (attrName "pinned", fg green)
                              , (attrName "selected", withStyle defAttr reverseVideo)
                              , (attrName "OP", fg blue)
                              , (attrName "rest", defAttr)
                              , (attrName "bar", fg yellow)
                              , (attrName "label", fg cyan)
                              , (hBorderAttr <> attrName "standard", withStyle defAttr dim)
                              ]

  in App
     { appDraw = drawTui
     -- , appChooseCursor = showFirstCursor
     , appChooseCursor = neverShowCursor
     , appHandleEvent = handleTuiEvent
     , appStartEvent = return ()
     , appAttrMap = const attrs
     }


-- draws the entire TuiState
drawTui :: TuiState -> [Widget ResourceName]
drawTui tui =
  let time = tui ^. currentTime
      order = tui ^. timeOrder
      topics = tui ^. topicList
      cats = tui ^. categoryList
      
  in case tui ^. displayState of
    DS d -> display time order topics cats d
    DSHelp old -> displayHelp (tui ^. baseURL) order topics old
    

display :: UTCTime -> TimeOrder -> TopicList -> CategoryList -> Display -> [Widget ResourceName]
display time _ topics _ DisplayAllTopics = displayAllTopics "Recent" time topics
display time order _ _ (DisplayTopic st) = displayTopic time st order
display time order _ _ (DisplayPost st) = displayPost time st order

display time _ _ cats DisplayAllCategories = displayCategories time cats
display time _ _ _ (DisplayCategory _ _ name topics) = displayAllTopics name time topics

display time order _ _ (DisplayCategoryTopic _ _ _ _ st) = displayTopic time st order
display time order _ _ (DisplayCategoryPost _ _ _ _ st) = displayPost time st order


-- I don't think this actually changes anything, but leave in for now
-- (the use of the standard attribute).
--
dimHorizontal :: Widget n
dimHorizontal = withAttr (hBorderAttr <> attrName "standard") hBorder


labelledLine :: T.Text -> Widget n
labelledLine lbl =
  let cts = hBorder <+> txt (" " <> lbl <> " ") <+> hBorder
  in withAttr (hBorderAttr <> attrName "standard") cts


displayAllTopics :: T.Text -> UTCTime -> TopicList -> [Widget ResourceName]
displayAllTopics lbl time topics =
  [labelledLine ("Topics : " <> lbl) <=>
   WL.renderList drawTopic True topics <=>
   helpBar time Nothing Nothing]
    where
        drawTopic selected tpc
          = (if tpc ^. pinned then withAttrName "pinned" else id)
            . padRight Max
            $ (likes' <+> title' <+> lastMod) <=>
              hBox [category', postsCount', posters'] <=>
              dimHorizontal
          where
                lastMod =
                  case tpc ^. lastUpdated of
                    Just lt -> padLeft Max
                               . padRight (Pad 1)
                               $ txt (showTimeDelta time lt)
                    Nothing -> emptyWidget

                hSize = 4
                shift = padLeft (Pad (hSize + 1))

                likes' :: Widget ResourceName
                likes' = (if selected then withAttrName "selected" else id)
                         . padRight (Pad 1)
                         . hLimit hSize
                         . padRight Max
                         . txt
                         . showInt
                         $ tpc ^. likeCount

                title' :: Widget ResourceName
                title' = withAttrName "title" . txt $ tpc ^. title

                postsCount' :: Widget ResourceName
                postsCount' = shift (countLabel "posts" (tpc ^. postsCount))

                posters' :: Widget ResourceName
                posters' = shift
                       . hBox
                       . mapFst (withAttrName "OP") (withAttrName "rest")
                       . showItems
                       $ tpc ^. posters

                category' :: Widget ResourceName
                category' = shift . txt $ tpc ^. category

                -- this could perhaps be re-worked now using Vector
                showItems :: V.Vector T.Text -> [Widget ResourceName]
                showItems v = map txt . V.toList $ (V.map (<> " ") . V.init $ v) V.++ V.singleton (V.last v)


displayCategories :: UTCTime -> CategoryList -> [Widget ResourceName]
displayCategories time cats =
  [labelledLine "Categories" <=>
   renderCategory <=>
   WL.renderList drawCategory True cats <=>
   helpBar time Nothing Nothing]
    where
      renderCategory = case snd <$> WL.listSelectedElement cats of
        Just cat ->
          padBottom Max (txtWrap (cat ^. categoryDesc)
                         <=> subCats cat
                         <=> dimHorizontal)

        Nothing -> emptyWidget
        
      drawCategory selected cat
        = catId selected cat
          <+> catName cat
          <+> padLeft Max (nums cat)

      hSize = 4
  
      catId selected cat =
        let shower = if selected then withAttrName "selected" else id
        in shower
           . padRight (Pad 1)
           . hLimit hSize
           . padRight Max
           . txt
           . showInt
           $ cat ^. categoryId

      catName cat = withAttrName "title" (txt (cat ^. categoryName))
      nums cat =
        countLabel "posts" (cat ^. categoryNPost) <+>
        padLeft (Pad 1) (countLabel "topics" (cat ^. categoryNTopic))

      subCats cat =
        let subs = zip [1..] (V.toList (cat ^. subCategoryList))

            addDesc scat = case scat ^. subcategoryDesc of
              Just desc -> padLeft (Pad 4) (txt desc)
              Nothing -> emptyWidget

            showSub (i, scat) =
              let l1 = padLeft (Pad 2) (txt (showInt i)) <+>
                       txt " " <+>
                       txt (scat ^. subcategoryName)
                  l2 = addDesc scat
              in l1 <=> l2

            processed = map showSub subs
            out = foldl' (<=>) emptyWidget processed
              
        in (if null subs then emptyWidget else txt " ") <=> out


countLabel :: T.Text -> Int -> Widget ResourceName
countLabel lbl n =
  withAttrName "label" (txt (lbl <> ":")) <+>
  padLeft (Pad 1) (txt (showInt n))


-- Can we send in the post title here?
displayTopic :: UTCTime -> SingleTopic -> TimeOrder -> [Widget ResourceName]
displayTopic time st order
    = [labelledLine ("Post : " <> st ^. stTitle)
       <=> selectedPost
       <=> WL.renderList drawPost True posts'
       <=> helpBar time (Just order) (st ^. stDownload)]
    where
        allPosts = st ^. stList
        posts' = orderSelect order allPosts

        selectedPost = case snd <$> WL.listSelectedElement allPosts of
          Just post ->
            let contents' = txtWrap (post ^. contents)

                {- errors out with infinite-width/height widgets
                border' = addScroll
                          . viewport PostView Both 
                          . padBottom Max
                          . padRight  Max
                -}
                
                border' = padBottom Max
                          . padRight  Max
                          
            in border' contents'
               <=> dimHorizontal
          _ -> emptyWidget

        -- We need space to get the number/score label but we don't really
        -- want to indent the following by that much.
        --
        drawPost selected post
            = postWidget
            where
                identifier = withAttrName (if selected then "selected" else "")
                  (hLimit 6 . padRight Max . txt $ postIdentifier post)

                userName'' = withAttrName "OP" . txt $ post ^. opUserName
                created = withAttrName "title"
                          . padLeft Max
                          . padRight (Pad 1)
                          $ txt (showTimeDelta time (post ^. opCreatedAt))
                firstLine = userName'' <+> created

                postWidget = identifier <+> firstLine


displayPost :: UTCTime -> SingleTopic -> TimeOrder -> [Widget ResourceName]
displayPost time st order = [showSelectedPost time order posts'']
  where
    allPosts = view stList st

    -- We need to invert the count if order is Descending.
    --
    posts' = WL.listReverse allPosts
    posts'' = case order of
      Increasing -> allPosts
      Decreasing -> case WL.listSelected allPosts of
        Just cPos -> WL.listMoveTo cPos posts'
        Nothing -> posts'


addScroll :: Widget n -> Widget n
addScroll = withHScrollBars OnBottom .
            withVScrollBars OnRight


showSelectedPost :: UTCTime -> TimeOrder -> WL.List ResourceName Post -> Widget ResourceName
showSelectedPost tNow order allPosts =
  case WL.listSelectedElement allPosts of
    (Just (ctr, thisPost)) ->
      let topBar = txt (postIdentifier thisPost <>
                        " " <> thisPost ^. opUserName)
                   <+> padLeft Max (txt (showTimeDelta tNow (thisPost ^. opCreatedAt)))

          nPosts = listLength allPosts

          -- Make the contents scrollable vertically.
          -- Can we identify when the contents exceed the viewport
          -- so we can add some decoration?
          --
          contents' = viewport SinglePostView Both
                      (txt $ thisPost ^. contents)
                      -- (txtWrap $ thisPost ^. contents)

      in withAttrName "OP" topBar
         <=> padBottom Max (addScroll contents')
         <=> helpPostBar order (ctr + 1) nPosts

    Nothing -> txt "something went wrong"


displayHelp ::
  String
  -> TimeOrder
  -> TopicList
  -> Display
  -> [Widget ResourceName]
displayHelp url order topics prev =
  let header = "View the discourse for " <> T.pack url

      -- Assume we have to have a selected element. This needs to be re-worked
      -- now we have multiple views.
      --
      Just tpc = view _2 <$> WL.listSelectedElement topics
      tstxt = T.intercalate "\n" [ "Selected topic:   " <> tpc ^. title
                                 , "Number of topics: " <> showInt (listLength topics)
                                 , "Number of posts:  " <> showInt (tpc ^. postsCount)
                                 , "Sort order:       " <> showOrder order
                                 , ""
                                 , T.pack getVersion
                                 ]

      help = "Right and left arrows move deeper into, or out of, topics.\n" <>
             "Up and down arrows move to earlier and later posts when viewing " <>
             "a single topic; shift + up/down scrolls a single post.\n\n" <>
             "c switches to the concept view, which looks like the topics " <>
             "view but can only be entered from the start page.\n\n" <>
             "s switches the time order between increasing and decreasing, " <>
             "which is used for the list of topics and posts views.\n\n" <>
             "v will show the selected post, topic, or list of topics " <>
             "in a web browser (only if the system supports 'gio open', so " <>
             "just Linux).\n\n" <>
             "h toggles this page and q exits the program."

      prevLabel = case prev of
                    DisplayAllTopics -> "topics"
                    DisplayTopic _ -> "topic"
                    DisplayPost _ -> "post"
                    DisplayAllCategories -> "categories"
                    DisplayCategory {} -> "category"
                    DisplayCategoryTopic {} -> "category topic"
                    DisplayCategoryPost {} -> "category topic"

      bottomBar = "h to return to " <> prevLabel <> " | q to quit"

  in [withAttrName "title" (txt header)
      <=> padTop (Pad 1) (txt tstxt)
      <=> padTop (Pad 1) (padBottom Max (txtWrap help))
      <=> withAttrName "bar" (txt bottomBar)]


-- The post number and the score
postIdentifier :: Post -> T.Text
postIdentifier thisPost =
  showInt (thisPost ^. postNumber) <> ":" <> showInt (thisPost ^. likes)


showInt :: Int -> T.Text
showInt = T.pack . show

showTimeDelta ::
  UTCTime
  -- ^ The current time
  -> UTCTime
  -- ^ A time in the past
  -> T.Text
showTimeDelta now old =
  let dt = diffUTCTime old now
  in LT.toStrict (F.format (FT.diff True) dt)


mapFst :: (a -> a) -> (a -> a) ->  [a] -> [a]
mapFst fn fn' (x:xs) = fn x : map fn' xs
mapFst _ _ [] = [] -- make this exhaustive


updateTime :: TuiState -> IO TuiState
updateTime tui = do
  now <- getCurrentTime
  pure $ tui & currentTime .~ now


addToList :: WL.List n e -> V.Vector e -> WL.List n e
addToList olist xs =
  let osel = WL.listSelected olist
      nelems = WL.listElements olist V.++ xs

  in WL.listReplace nelems osel olist



-- Do we have any downloading to process?
-- We need to kill any downloads if we are moving back to the topic list
--
-- It would be cleaner to just hold the TUI until the download has happened.
--
-- Does this work?
--

downloadTuiEvent ::
  UTCTime
  -> TopicList
  -> CategoryList
  -> M.IntMap Category
  -> String
  -> TimeOrder
  -> (SingleTopic -> Display)
  -> SingleTopic
  -> ExtraDownload
  -> EventM m TuiState ()
downloadTuiEvent ct tl cl cmap burl to dt st ed = do

  -- Have we downloaded the data yet?
  --
  rsp <- liftIO (tryTakeMVar (ed ^. edQuery))
  case rsp of
    Nothing -> pure ()
    Just nposts -> do

      ned <- liftIO (getExtraPosts (ed ^. edBaseUrl) (ed ^. edToDo))

      let nlist = addToList (st ^. stList) nposts
          nst = st & stList .~ nlist & stDownload .~ ned

      put (TuiState ct tl cl cmap burl to (DS (dt nst)))


-- Enter the selected topic.
--
toTopic :: TuiState -> EventM ResourceName TuiState ()
toTopic tui = do
  st <- liftIO $ getPosts tui (tui ^. topicList)
  let ndisplay = if st ^. stNumPosts == 1 then DisplayPost st else DisplayTopic st
  put $ tui & displayState .~ DS ndisplay


-- Try to ensure any download is cancelled.
--
fromTopic :: TuiState -> SingleTopic -> EventM ResourceName TuiState()
fromTopic tui st = do
  case st ^. stDownload of
    Nothing -> pure ()
    Just dl -> liftIO (do
                          flag <- isEmptyMVar (dl ^. edQuery)
                          unless flag (killThread (dl ^. edTID)))

  put $ tui & displayState .~ DS DisplayAllTopics


-- Enter the selected post.
--
toPost :: TuiState -> SingleTopic -> EventM ResourceName TuiState ()
toPost tui st =
  put $ tui & displayState .~ DS (DisplayPost st)


-- Leaving a post, do we want to show the topic list or all topics?
--
-- TODO: should this delete any existing download? It's probably okay
--       since if a single post then there won't be a download and if
--       multiple posts then we want to keep the download going.
--
fromPost :: TuiState -> SingleTopic -> EventM ResourceName TuiState ()
fromPost tui st =
  let ndisplay = if st ^. stNumPosts == 1
                 then DisplayAllTopics
                 else DisplayTopic st

  in put $ tui & displayState .~ DS ndisplay


-- Enter the selected category.
---
toCategory :: TuiState -> EventM ResourceName TuiState ()
toCategory tui = do
  ndisplay <- liftIO $ getCategory tui
  put $ tui & displayState .~ DS ndisplay


-- Enter the selected category topic
--
toCatTopic :: TuiState -> Slug -> Int -> T.Text -> TopicList -> EventM ResourceName TuiState ()
toCatTopic tui slug idVal name tl = do
  st <- liftIO $ getPosts tui tl
  let ndisplay = if st ^. stNumPosts == 1 then DisplayCategoryPost else DisplayCategoryTopic
  put $ tui & displayState .~ DS (ndisplay slug idVal name tl st)


-- Enter the selected category post.
--
toCatPost :: TuiState -> Slug -> Int -> T.Text -> TopicList -> SingleTopic -> EventM ResourceName TuiState ()
toCatPost tui slug idVal name tl st =
  put $ tui & displayState .~ DS (DisplayCategoryPost slug idVal name tl st)


-- This doesn't feel right, but that's in part becasue I don't understand
-- the logic of when scrollHandler should be used or not.
--
isPost :: TuiState -> Bool
isPost tui =
  case tui ^. displayState of
    DS (DisplayPost {}) -> True
    DS (DisplayCategoryPost {}) -> True
    _ -> False


handleTuiEvent :: BrickEvent ResourceName e -> EventM ResourceName TuiState ()
handleTuiEvent (VtyEvent (EvKey (KChar 'q') _)) = halt

handleTuiEvent e = do
  tui <- get

  -- Are we still downloading?
  --
  let isDownloading (DisplayTopic st) = checkDownload DisplayTopic st
      isDownloading (DisplayPost st) = checkDownload DisplayPost st
      -- What to do about the category cases?
      isDownloading (DisplayCategoryTopic a b c d st) = checkDownload (DisplayCategoryTopic a b c d) st
      isDownloading (DisplayCategoryPost a b c d st) = checkDownload (DisplayCategoryPost a b c d) st
      isDownloading _ = Nothing

      checkDownload dt st = case st ^. stDownload of
                              Just ed -> Just (dt, st, ed)
                              Nothing -> Nothing

      -- This loses the event if we are downloading. Perhaps
      -- we should process the event after the download? However,
      -- there could be a lot of them, so it may make sense to
      -- just drop them.
      --
      process d = case isDownloading d of
                    Just (dt, st, ed) -> downloadTuiEvent (tui ^. currentTime) (tui ^. topicList) (tui ^. categoryList) (tui ^. categoryMap) (tui ^. baseURL) (tui ^. timeOrder) dt st ed
                    Nothing -> handleTuiEvent' tui e

  case tui ^. displayState of
    DS d -> process d
    DSHelp d -> process d


handleTuiEvent' :: TuiState -> BrickEvent ResourceName e -> EventM ResourceName TuiState ()

-- h toggles help
handleTuiEvent' tui (VtyEvent (EvKey (KChar 'h') _)) = do
  ntui <- liftIO (updateTime tui)

  let nstate = case ntui ^. displayState of
        DS d -> DSHelp d
        DSHelp d -> DS d
        
  let ntui' = ntui & displayState .~ nstate
  put ntui'


-- We now change the setting whatver is being displayed (previously
-- we required a single topic to be selected.
--
handleTuiEvent' tui (VtyEvent (EvKey (KChar 's') _)) =
  let new = case tui ^. timeOrder of
              Decreasing -> Increasing
              Increasing -> Decreasing

      ntui = tui & timeOrder .~ new

  in put ntui


-- The v keypress will load the URL in a web browser (Linux only,
-- and only those that support the gio command). The URL
-- depends on what is being shown
--
--    all topics     -> /latest
--    topic          -> /t/slug/id
--    post           -> /t/slug/id/number
--    all categories -> /categories
--    category       -> /c/slug/it
--
--
handleTuiEvent' (TuiState _ _ _ _ burl to (DS ds)) (VtyEvent (EvKey (KChar 'v') _)) = do

  let frag = case ds of
               DisplayAllTopics -> "latest"
               DisplayTopic st -> getBase st
               DisplayPost st -> getBase st <> "/" <> showInt (getN st)
               DisplayAllCategories -> "categories"
               DisplayCategory slug slugId _ _ -> "c/" <> slug <> "/" <> showInt slugId

               -- Follow DisplayTopic/Post
               DisplayCategoryTopic _ _ _ _ st -> getBase st
               DisplayCategoryPost _ _ _ _ st -> getBase st <> "/" <> showInt (getN st)

      getBase st = "t/" <> st ^. stSlug <> "/" <> showInt (st ^. stId)

      getN st =
        let plist = st ^. stList
        in case snd <$> WL.listSelectedElement (orderSelect to plist) of
             Just post -> post ^. postNumber
             Nothing -> 1

  liftIO (showPage burl frag)


-- Handle movement in the single-post view:
--  - do we move within the page (up/down, pgup,pgdown, home/end)
--  - do we move to the next or previous posts?
--
-- We could be clever but instead 'cheat' and use the shift key to indicate
-- the shifts are *within* the page and no-specifier (well, technically
-- all other ones) to indicate between posts
--
-- Unfortunately my WM seams to eat up the pgup/down/home/end key presses.
--
handleTuiEvent' tui (VtyEvent (EvKey k [MShift]))
  = let scroll =
          let vp = viewportScroll SinglePostView
          in case k of
            KUp -> vScrollBy vp (-1)
            KDown -> vScrollBy vp 1

            KRight -> hScrollBy vp 1
            KLeft -> hScrollBy vp (-1)

            -- gargh - not getting the following to work ...
            KPageUp -> vScrollPage vp Up
            KPageDown -> vScrollPage vp Down
            KHome -> vScrollToBeginning vp
            KEnd -> vScrollToEnd vp
            _ -> pure ()

    in case tui ^. displayState of
      DS (DisplayPost _) -> scroll
      DS (DisplayCategoryPost {}) -> scroll
      _ -> pure ()


-- Note: we don't want to eat up the up/down events for other cases.
--
-- However, how does this work with the default scrollHandlers?
--
handleTuiEvent' tui (VtyEvent (EvKey k _)) | k `elem` [KUp, KDown] && isPost tui
  = let -- If we didn't want to make up always go backwards in time
        -- then we could just check on k.
        step = case (k, tui ^. timeOrder) of
          (KUp, Increasing) -> -1
          (KDown, Decreasing) -> -1
          _ -> 1

        scroll d st =
          let nlist = WL.listMoveBy step (st ^. stList)
              nst = st & stList .~ nlist

          in tui & displayState .~ DS (d nst)

    in case tui ^. displayState of
      DS (DisplayPost st) -> put $ scroll DisplayPost st
      DS (DisplayCategoryPost slug idVal name ctl st) -> put $ scroll (DisplayCategoryPost slug idVal name ctl) st
      _ -> pure ()  -- this can not happen


-- What happens if we press right?
--
handleTuiEvent' tui (VtyEvent (EvKey KRight _)) = do
  ntui <- liftIO (updateTime tui)
  case tui ^. displayState of
    DSHelp _ -> put ntui
    DS DisplayAllTopics -> toTopic ntui
    DS (DisplayTopic st) -> toPost ntui st
    DS (DisplayPost _) -> put ntui
    DS DisplayAllCategories -> toCategory ntui
    DS (DisplayCategory slug idVal name tl) -> toCatTopic ntui slug idVal name tl
    DS (DisplayCategoryTopic slug idVal name tl st) -> toCatPost ntui slug idVal name tl st
    DS (DisplayCategoryPost {}) -> put ntui


-- What happens if we press left?
--
-- TODO: When do we want to kill of any existing download?
--
handleTuiEvent' tui (VtyEvent (EvKey KLeft _)) = do
  ntui <- liftIO (updateTime tui)
  case tui ^. displayState of
    DSHelp _ -> put ntui
    DS DisplayAllTopics -> put ntui
    DS (DisplayTopic st) -> fromTopic ntui st
    DS (DisplayPost st) -> fromPost ntui st
    DS DisplayAllCategories -> put (ntui & displayState .~ DS DisplayAllTopics)
    DS DisplayCategory {} -> put (ntui & displayState .~ DS DisplayAllCategories)
    DS (DisplayCategoryTopic slug idVal name tl _) -> put (ntui & displayState .~ DS (DisplayCategory slug idVal name tl))

    DS (DisplayCategoryPost slug idVal name tl st) ->
      let ndisplay = if st ^. stNumPosts == 1
                     then DisplayCategory slug idVal name tl
                     else DisplayCategoryTopic slug idVal name tl st
      in put (ntui & displayState .~ DS ndisplay)


-- When can we view the categories? For the moment just in the all-display, but
-- this could be anytime.
--
handleTuiEvent' (TuiState _ tl cl cmap burl to (DS DisplayAllTopics)) (VtyEvent (EvKey (KChar 'c') _)) = do
  nct <- liftIO getCurrentTime
  put (TuiState nct tl cl cmap burl to (DS DisplayAllCategories))


-- Should we update the time in these cases? It would make the display
-- "reactive", but it also might be confusing when scrolling to see
-- the times change.
--
handleTuiEvent' tui@(TuiState _ tl _ _ _ _ (DS DisplayAllTopics)) ev
    = scrollHandler (\x -> tui & topicList .~ x) tl ev

handleTuiEvent' tui@(TuiState _ _ cats _ _ _ (DS DisplayAllCategories)) ev
    = scrollHandler (\x -> tui & categoryList .~ x) cats ev

handleTuiEvent' tui@(TuiState _ _ _ _ _ _ (DS (DisplayTopic st))) ev
  = let list = st ^. stList
    in scrollHandler (\x -> tui & displayState .~ DS (DisplayTopic (st & stList .~ x))) list ev

handleTuiEvent' tui@(TuiState _ _ _ _ _ _ (DS (DisplayCategory slug slugId name tl))) ev
  = scrollHandler (\x -> tui & displayState .~ DS (DisplayCategory slug slugId name x)) tl ev

handleTuiEvent' tui@(TuiState _ _ _ _ _ _ (DS (DisplayCategoryTopic slug slugId name tl st))) ev
  = let list = st ^. stList
    in scrollHandler (\x -> tui & displayState .~ DS (DisplayCategoryTopic slug slugId name tl (st & stList .~ x))) list ev


-- nothing else to do so update the time
handleTuiEvent' tui _ = liftIO (updateTime tui) >>= put


listLength :: WL.List n a -> Int
listLength = V.length . WL.listElements

-- | Try to load the page into a web browser. This is highly OS dependent.
showPage :: String -> T.Text -> IO ()
showPage base frag = void $ spawnProcess "gio" ["open", base <> T.unpack frag]


scrollHandler ::
  Ord n
  => (WL.List n e -> s)
  -> WL.List n e
  -> BrickEvent m f
  -> EventM n s ()
scrollHandler restoreTuiState list (VtyEvent ev) = do
  nlist <- nestEventM' list (WL.handleListEvent ev)
  put (restoreTuiState nlist)

scrollHandler _ _ _ = pure ()  -- assume we do nothing
