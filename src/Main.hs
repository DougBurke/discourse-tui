{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Brick.Widgets.List as WL

import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V

import qualified Formatting as F
import qualified Formatting.Time as FT

import Brick (BrickEvent(..), App(..), EventM, Next, Padding(..), ViewportType(Vertical), Widget,
              Direction(..),
              (<=>), (<+>),
              attrMap, continue, defaultMain,
              fg,
              halt, hBox, hLimit,
              neverShowCursor,
              padBottom, padLeft, padRight, padTop,
              txt, txtWrap,
              viewport, viewportScroll,
              vScrollBy, vScrollToBeginning, vScrollToEnd, vScrollPage,
              vLimit,
              withAttr, withBorderStyle)
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (hCenter)

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (isEmptyMVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Exception (evaluate)
import Control.Lens (Getting
                    , (&), (?~), (.~), (^.), _2
                    , view)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)

import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)

import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(MShift))
import Graphics.Vty.Attributes (blue, bold, defAttr, green, reverseVideo, withStyle, yellow)

import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest, setRequestQueryString)

import System.Environment (getArgs)
import System.Exit (die)
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

helpMessage :: String
helpMessage = intercalate "\n"
  [ "Usage: discourse-tui url|fragment"
  , ""
  , "where fragment (no . character) is taken to mean https://discource.fragment.org,"
  , "or the full URL is given. So either:"
  , ""
  , "     discource-tui haskell"
  , "     discourse-tui https://discourse.haskell.org"
  ]

parseArgs :: IO String
parseArgs = do
    args <- getArgs
    case args of
      [x] | x /= "--help" -> pure $ if '.' `elem` x
                                    then x
                                    else "https://discourse." ++ x ++ ".org"
      _ -> die helpMessage


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
  categoriesRequest <- parseRequest (baseUrl' <> "categories.json")
  categoriesResp <- getResponseBody <$> httpJSON categoriesRequest
  tps <- getResponseBody <$> httpJSON topicsRequest

  let users = tps ^. tpUsers
      topicList = tps ^. tpTopicList

      tokenize :: Getting M.Key a M.Key -> V.Vector a -> M.IntMap a
      tokenize f = M.fromList . V.toList . V.map (\x -> (x ^. f, x))

      userMap = tokenize userId users
      categoryMap = tokenize categoryId $ categoriesResp ^. categories

      widgetList = V.map (parseTopic userMap categoryMap) topicList
      topics' = WL.list Contents widgetList topicHeight

  now <- getCurrentTime
  pure TuiState {
    _currentTime = now
    , _topics = topics'
    , _posts = Nothing
    , _baseURL = baseUrl'
    , _timeOrder = Increasing
    , _displayState = initDisplayState
    }

-- The help bar at the bottom, for most pages.
--
helpBase :: T.Text
helpBase = "h help | q to quit"

helpBar :: Maybe TimeOrder -> Bool -> Widget ResourceName
helpBar Nothing _ = withAttr "bar" (txt helpBase)
helpBar (Just order) True = withAttr "bar" widget
  where
    widget = txt helpBase <+> downloading <+> dirMsg
    dirMsg = padLeft Max $ txt (showOrder order)
    downloading = hCenter (txt "... downloading ...")

helpBar (Just order) False = withAttr "bar" widget
  where
    widget = txt helpBase <+> dirMsg
    dirMsg = padLeft Max $ txt (showOrder order)

-- The help bar for the single-post page
helpPostBar :: TimeOrder -> Int -> Int -> Widget ResourceName
helpPostBar order cur nposts = withAttr "bar" widget
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
-- uses t/topcid.json which returns the first set of data and
-- a list of all the posts in the topic. We then set up a
-- thread to download the remaining data, which will be checked
-- by handleTuiEvent.
--
getPosts ::
  TuiState
  -> IO SingleTopic
getPosts ts = do
    let Just selectedTopicID = view (_2 . topicId) <$> WL.listSelectedElement (ts ^. topics)
        postURL = mconcat [ts ^. baseURL, "t/", show selectedTopicID, ".json"]
        extraURL = mconcat [ts ^. baseURL, "t/", show selectedTopicID, "/posts.json"]

    postsRequest <- parseRequest postURL
    pr <- getResponseBody <$> httpJSON postsRequest
    basePosts <- mapM postToPandoc (pr ^. postList)

    let allIds = S.fromList . V.toList $ pr ^. postIds
        gotIds = S.fromList . V.toList . V.map (^. postId) $ pr ^. postList
        extraIds = V.fromList . S.toAscList $ allIds `S.difference` gotIds

        nposts = V.length (pr ^. postIds)

    mExtra <- getExtraPosts extraURL extraIds
    pure $ toSingleTopic (pr ^. postResponseId) nposts (pr ^. postSlug) (WL.list Posts basePosts 10) mExtra


getExtraPosts :: String -> V.Vector Int -> IO (Maybe ExtraDownload)
getExtraPosts _ ids | V.null ids = pure Nothing
getExtraPosts baseUrl ids = do
  let (now, later) = V.splitAt 20 ids

      getChunk xs = do
        let qry = map (\x -> ("post_ids[]", toN x)) (V.toList xs)
            toN = Just . B.pack . show
        req <- parseRequest baseUrl
        let req' = setRequestQueryString qry req
        rsp <- getResponseBody <$> httpJSON req'
        pure (rsp ^. postSelList)

  mvar <- newEmptyMVar
  tid <- forkIO $ do
    posts' <- getChunk now >>= mapM postToPandoc >>= evaluate
    putMVar mvar posts'

  pure . Just $ toExtraDownload later mvar tid


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


-- This reverses the list *BUT* keeps the selected element,
-- which is a bit odd, and up/down still follow the ordering
-- of the original list, which is also odd.
--
-- Changing the selected position should be possible, but it
-- needs to be done when the ordering is switched, and then
-- that is a problem as the order is global but there are
-- two lists it's used with. Aha. I have decided to only
-- apply it to the posts lists, which makes things a it
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
  let attrs = attrMap mempty [ ("title", withStyle defAttr bold)
                             , ("pinned", fg green)
                             , ("selected", withStyle defAttr reverseVideo)
                             , ("OP", fg blue)
                             , ("rest", defAttr)
                             , ("bar", fg yellow) ]

  in App
     { appDraw = drawTui
     -- , appChooseCursor = showFirstCursor
     , appChooseCursor = neverShowCursor
     , appHandleEvent = handleTuiEvent
     , appStartEvent = pure
     , appAttrMap = const attrs
     }


-- draws the entire TuiState
drawTui :: TuiState -> [Widget ResourceName]
drawTui tui =
  case getDisplayState (tui ^. displayState) of
    DisplayAllTopics -> displayAllTopics tui
    DisplayTopic -> displayTopic tui
    DisplayPost -> displayPost tui
    DisplayHelp -> displayHelp tui


displayAllTopics :: TuiState -> [Widget ResourceName]
displayAllTopics tui =
  [WL.renderList drawTopic True (tui ^. topics) <=> helpBar Nothing False]
    where
        drawTopic selected tpc
          = withBorderStyle unicodeRounded
            . border
            . (if tpc ^. pinned then withAttr "pinned" else id)
            . padRight Max
            $ (likes' <+> title' <+> lastMod) <=>
            hBox [category', postsCount', posters']
          where
                lastMod = padLeft Max
                          . padRight (Pad 1)
                          $ txt (showTimeDelta (tui ^. currentTime) (tpc ^. lastUpdated))

                likes' :: Widget ResourceName
                likes' = (if selected then withAttr "selected" else id)
                         . padRight (Pad 1)
                         . hLimit 4
                         . padRight Max
                         . txt
                         . T.pack
                         . show
                         $ tpc ^. likeCount

                title' :: Widget ResourceName
                title' = withAttr "title" . txt $ tpc ^. title

                postsCount' :: Widget ResourceName
                postsCount' = padLeft (Pad 5)
                            . txt
                            . ("posts: " <>)
                            . T.pack
                            . show
                            $ tpc ^. postsCount

                posters' :: Widget ResourceName
                posters' = padLeft (Pad 5)
                       . hBox
                       . mapFst (withAttr "OP") (withAttr "rest")
                       . showItems
                       $ tpc ^. posters

                category' :: Widget ResourceName
                category' = padLeft (Pad 5) . txt $ tpc ^. category

                -- this could perhaps be re-worked now using Vector
                showItems :: V.Vector T.Text -> [Widget ResourceName]
                showItems v = map txt . V.toList $ (V.map (<> " ") . V.init $ v) V.++ V.singleton (V.last v)


displayTopic :: TuiState -> [Widget ResourceName]
displayTopic tui = [renderTopic tui]

displayPost :: TuiState -> [Widget ResourceName]
displayPost tui = [renderPost tui]

displayHelp :: TuiState -> [Widget ResourceName]
displayHelp tui = [renderHelp tui]


renderTopic :: TuiState -> Widget ResourceName
renderTopic tui
    = WL.renderList drawPost True posts'
      <=> helpBar (Just order) (isJust (st ^. stDownload))
    where
        Just st = tui ^. posts
        allPosts = st ^. stList
        order = tui ^. timeOrder
        posts' = orderSelect order allPosts

        -- We need space to get the number/score label but we don't really
        -- want to indent the following by that much.
        --
        drawPost selected post
            = border'
            $ withAttr (if selected then "selected" else "")
              (hLimit 6 . padRight Max . txt $ postIdentifier post)
            <+> ((userName'' <+> created)
                 <=> contents')
            where
                userName'' = withAttr "OP" . txt $ post ^. opUserName
                created = withAttr "title"
                          . padLeft Max
                          . padRight (Pad 1)
                          $ txt (showTimeDelta (tui ^. currentTime) (post ^. opCreatedAt))
                contents' = txtWrap (post ^. contents)
                border' = withBorderStyle unicodeRounded
                          . border
                          . vLimit 8
                          . padBottom Max
                          . padRight  Max


renderPost :: TuiState -> Widget ResourceName
renderPost tui = showSelectedPost (tui ^. currentTime) order posts''
  where
    Just allPosts = view stList <$> tui ^. posts  -- must be a Just
    order = tui ^. timeOrder

    -- We need to invert the count if order is Descending.
    --
    posts' = WL.listReverse allPosts
    posts'' = case order of
      Increasing -> allPosts
      Decreasing -> case WL.listSelected allPosts of
        Just cPos -> WL.listMoveTo cPos posts'
        Nothing -> posts'


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
          contents' = viewport SinglePostView Vertical (txtWrap $ thisPost ^. contents)

      in withAttr "OP" topBar
         <=> padBottom Max contents'
         <=> helpPostBar order (ctr + 1) nPosts

    Nothing -> txt "something went wrong"


renderHelp :: TuiState -> Widget ResourceName
renderHelp tui =
  let header = "View the discourse for " <> T.pack (tui ^. baseURL)

      -- assume we have to have a selected element
      ts = tui ^. topics
      Just tpc = view _2 <$> WL.listSelectedElement ts
      tstxt = T.intercalate "\n" [ "Selected topic:   " <> tpc ^. title
                                 , "Number of topics: " <> showInt (listLength ts)
                                 , "Number of posts:  " <> showInt (tpc ^. postsCount)
                                 , "Sort order:       " <> showOrder (tui ^. timeOrder)
                                 ]

      help = "Right and left arrows move deeper into, or out of, topics.\n" <>
             "Up and down arrows move to earlier and later posts when viewing " <>
             "a single topic; shift + up/down scrolls a single post.\n\n" <>
             "s switches the time order between increasing and decreasing, " <>
             "which is used for the list of topics and posts views.\n\n" <>
             "v will show the selected post, topic, or list of topics " <>
             "in a web browser (only if the system supports 'gio open', so " <>
             "just Linux).\n\n" <>
             "h toggles this page and q exits the program."

      prev = case getPreviousDisplayState (tui ^. displayState) of
               Just DisplayPost -> "post"
               Just DisplayTopic -> "topic"
               Just DisplayAllTopics -> "topics"
               Just DisplayHelp -> "help"  -- should not occur
               Nothing -> "help" -- should not occur

      bottomBar = "h to return to " <> prev <> " | q to quit"

  in withAttr "title" (txt header)
     <=> padTop (Pad 1) (txt tstxt)
     <=> padTop (Pad 1) (padBottom Max (txtWrap help))
     <=> withAttr "bar" (txt bottomBar)


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


-- Are we downloading more posts?
isDownloading :: TuiState -> Bool
isDownloading tui =
  case tui ^. posts of
    Nothing -> False
    Just st -> isJust (st ^. stDownload)


addToList :: WL.List n e -> V.Vector e -> WL.List n e
addToList olist xs =
  let osel = WL.listSelected olist
      nelems = WL.listElements olist V.++ xs

  in WL.listReplace nelems osel olist


handleTuiEvent :: TuiState -> BrickEvent ResourceName e -> EventM ResourceName (Next TuiState)
handleTuiEvent tui (VtyEvent (EvKey (KChar 'q') _)) = halt tui


-- Do we have any downloading to process?
-- We need to kill any downloads if we are moving back to the topic list
--
-- It would be cleaner to just hold the TUI until the download has happened.
--
handleTuiEvent tui e | isDownloading tui = do
  let Just st = tui ^. posts
      Just ed = st ^. stDownload

  -- Have we downloaded the data yet?
  --
  rsp <- liftIO (tryTakeMVar (ed ^. edQuery))
  ntui <- case rsp of
    Nothing -> pure tui
    Just nposts -> do

      -- Recreating the URL is not great; we should have set up a constructor to
      -- hide this logic.
      --
      let Just selectedTopicID = view (_2 . topicId) <$> WL.listSelectedElement (tui ^. topics)
          extraURL = mconcat [tui ^. baseURL, "t/", show selectedTopicID, "/posts.json"]
      ned <- liftIO (getExtraPosts extraURL (ed ^. edToDo))

      let nlist = addToList (st ^. stList) nposts
          nst = st & stList .~ nlist & stDownload .~ ned

      pure $ tui & posts ?~ nst

  handleTuiEvent ntui e


-- h toggles help
handleTuiEvent tui (VtyEvent (EvKey (KChar 'h') _)) = do
  ntui <- liftIO (updateTime tui)

  let ostates = ntui ^. displayState
      Just nstates = updateDisplayState ostates DisplayHelp

  let ntui' = ntui & displayState .~ nstates
  continue ntui'


-- We only change the setting when the posts are listed, not
-- in all cases.
--
handleTuiEvent tui (VtyEvent (EvKey (KChar 's') _)) =
  let new = case tui ^. timeOrder of
              Decreasing -> Increasing
              Increasing -> Decreasing

      ntui = case getDisplayState (tui ^. displayState) of
        DisplayTopic -> tui & timeOrder .~ new  -- TODO check this is right
        _ -> tui

  in continue ntui


-- The v keypress will load the URL in a web browser (Linux only,
-- and only those that support the gio command):
--
-- topics, posts = Nothing, singlePostView = _ -> /latest
-- topics, posts = Just _, singlePostView is False -> /t/slug/id
-- topics, posts = Just _, singlePostView is True -> /t/slug/id/number
--
handleTuiEvent tui (VtyEvent (EvKey (KChar 'v') _)) = do

  let frag = case getDisplayState (tui ^. displayState) of
               DisplayTopic -> base
               DisplayPost -> base <> "/" <> showInt n
               _ -> "latest"

      -- we only use stopic when we know this succeeds
      Just stopic = tui ^. posts
      plist = stopic ^. stList
      n = case WL.listSelectedElement (orderSelect (tui ^. timeOrder) plist) of
            Just (_, post) -> post ^. postNumber
            Nothing -> 1

      base = "t/" <> stopic ^. stSlug <> "/" <> showInt (stopic ^. stId)

  liftIO (showPage (tui ^. baseURL) frag)
  continue tui

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
handleTuiEvent tui (VtyEvent (EvKey k [MShift])) | getDisplayState (tui ^. displayState) == DisplayPost
  = let vp = viewportScroll SinglePostView

        op = case k of
          KUp -> vScrollBy vp (-1)
          KDown -> vScrollBy vp 1
          -- gargh - not getting the following to work ...
          KPageUp -> vScrollPage vp Up
          KPageDown -> vScrollPage vp Down
          KHome -> vScrollToBeginning vp
          KEnd -> vScrollToEnd vp
          _ -> pure ()
        
    in op >> continue tui
       

handleTuiEvent tui (VtyEvent (EvKey k _)) | getDisplayState (tui ^. displayState) == DisplayPost && k `elem` [KUp, KDown]
  = let Just posts' = tui ^. posts

        -- If we didn't want to make up always go backwards in time
        -- then we could just check on k.
        step = case (k, tui ^. timeOrder) of
          (KUp, Increasing) -> -1
          (KDown, Decreasing) -> -1
          _ -> 1

        nlist = WL.listMoveBy step (posts' ^. stList)
        ntui = tui & posts ?~ (posts' & stList .~ nlist)

    in continue ntui

handleTuiEvent tui (VtyEvent (EvKey KRight _)) | getDisplayState (tui ^. displayState) == DisplayAllTopics 
  = do
  ntui <- liftIO (updateTime tui)
  posts' <- liftIO $ getPosts ntui
  let display = if posts' ^. stNumPosts == 1 then DisplayPost else DisplayTopic
      Just nstate = updateDisplayState (tui ^. displayState) display
      ntui' = ntui & posts ?~ posts' & displayState .~ nstate
  continue ntui'

handleTuiEvent tui (VtyEvent (EvKey KRight _)) | getDisplayState (tui ^. displayState) == DisplayTopic 
  = do
  ntui <- liftIO (updateTime tui)
  let ntui' = ntui & displayState .~ nstate
      Just nstate = updateDisplayState (tui ^. displayState) DisplayPost
  continue ntui'

-- Left out of a post takes us to the topic UNLESS it's a single-post topic
-- Really I should worry about killing the download thread when swapping
-- to DisplayAllTopics **but** we know we don't have that case here as
-- there's only 1 post.
--
handleTuiEvent tui (VtyEvent (EvKey KLeft  _))
  | getDisplayState (tui ^. displayState) == DisplayPost
  = let Just posts' = tui ^. posts
        (nposts, display) = if posts' ^. stNumPosts == 1
                            then (Nothing, DisplayAllTopics)
                            else (Just posts', DisplayTopic)

        Just nstate = updateDisplayState (tui ^. displayState) display

    in do
      ntui <- liftIO (updateTime tui)
      continue $ ntui & posts .~ nposts & displayState .~ nstate


-- Left out of a topic takes us to the topic list and removes and
-- outstanding downloads. We could just let them finish.
--
handleTuiEvent tui (VtyEvent (EvKey KLeft  _)) |
  getDisplayState (tui ^. displayState) == DisplayTopic
  = let Just posts' = tui ^. posts

        Just nstate = updateDisplayState (tui ^. displayState) DisplayAllTopics

    in do

      case posts' ^. stDownload of
        Nothing -> pure ()
        Just dl -> do
          flag <- liftIO (isEmptyMVar (dl ^. edQuery))
          unless flag $ liftIO (killThread (dl ^. edTID))

      ntui <- liftIO (updateTime tui)
      let ntui' = ntui & posts .~ Nothing & displayState .~ nstate
      continue ntui'




-- Should we update the time in these cases? It would make the display
-- "reactive", but it also might be confusing when scrolling to see
-- the times change.
--
handleTuiEvent tui ev | getDisplayState (tui ^. displayState) == DisplayTopic
  = let Just posts' = tui ^. posts
        list = posts' ^. stList
    in scrollHandler (\x -> tui & posts ?~ (posts' & stList .~ x)) list ev

handleTuiEvent tui ev | getDisplayState (tui ^. displayState) == DisplayAllTopics
    = scrollHandler (\x -> tui & topics .~ x) (tui ^. topics) ev

-- nothing else to do so update the time
handleTuiEvent tui _ = liftIO (updateTime tui) >>= continue


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
  -> EventM n (Next s)
scrollHandler restoreTuiState list (VtyEvent ev) = continue . restoreTuiState =<< handler
    where
        handler = WL.handleListEvent ev list
scrollHandler restoreTuiState list _ = continue $ restoreTuiState list  -- is this correct?
