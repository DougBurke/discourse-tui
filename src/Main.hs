{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Brick.Widgets.List as WL

import qualified Data.IntMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V

import qualified Formatting as F
import qualified Formatting.Time as FT

import Control.Monad.IO.Class (liftIO)

import Brick
import Brick.Widgets.Border

import Control.Lens (Getting
                    , (&), (?~), (.~), (^.), _2, _3
                    , view)
import Control.Monad (void)

import Data.Maybe (isJust, isNothing)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)

import Graphics.Vty.Input.Events (Event(..), Key(..))
import Graphics.Vty.Attributes (blue, bold, defAttr, green, withStyle, yellow)

import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)

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
helpMessage = "Usage: discourse-tui url \n Ex: discourse-tui https://discourse.haskell.org"

parseArgs :: IO String
parseArgs = do
    args <- getArgs
    if null args || head args == "--help"
        then die helpMessage
        else pure $ head args

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
      topics' = WL.list "contents" widgetList topicHeight

  now <- getCurrentTime
  pure TuiState {
    _currentTime = now,
    _topics = topics',
    _posts = Nothing,
    _baseURL = baseUrl',
    _singlePostView = False,
    _timeOrder = Increasing
    }

-- The help bar at the bottom, for most pages.
--
helpBar :: Maybe TimeOrder -> Widget ResourceName
helpBar mOrder = withAttr "bar" widget
  where
    widget = case mOrder of
      Just order -> txt msgFull <+> dirMsg order
      Nothing -> txt msgBase

    msgBase = "arrow keys -> move | left right -> read replies/full post | q to quit"
    msgFull = "arrow keys -> move | left right -> read replies/full post | s swap order | q to quit"

    dirMsg order = padLeft Max $ txt (showOrder order)

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

    msgBase = "left -> full post | "
              <> (if nposts == 1
                  then ""
                  else "up/down -> previous/next | s swap order | ")
              <> "q to quit"


showOrder :: TimeOrder -> T.Text
showOrder Increasing = "↑" -- unicode 2191  "+"
showOrder Decreasing = "↓" -- unicode 2193  "-"


-- get the posts for the current topic
--
-- TODO: pagination, but for now just use ?print=true which is meant to get
--       up to 1000 posts, which should be good for me (as I can't see an obvious
--       pagination feature). Hmm, this seems to trigger user-access restrictions!
--
getPosts :: TuiState -> IO (Int, Slug, WL.List ResourceName Post)
getPosts ts = do
    let Just selectedTopicID = view (_2 . topicId) <$> WL.listSelectedElement (ts ^. topics)
        postURL = mconcat [ts ^. baseURL, "t/", show selectedTopicID, ".json" {- , "?print=true" -}]

    postsRequest <- parseRequest postURL
    pr <- getResponseBody <$> httpJSON postsRequest
    allPosts <- mapM postToPandoc (pr ^. postList)
    pure (pr ^. postResponseId, pr ^. postSlug, WL.list "posts" allPosts 10)

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
                             , ("selected", bg yellow)
                             , ("OP", fg blue)
                             , ("rest", defAttr)
                             , ("bar", fg yellow) ]

  in App
     { appDraw = drawTui
     , appChooseCursor = showFirstCursor
     , appHandleEvent = handleTuiEvent
     , appStartEvent = pure
     , appAttrMap = const attrs
     }


-- draws the entire TuiState
-- this pattern matches the topic list
drawTui :: TuiState -> [Widget ResourceName]
drawTui tui | isNothing (tui ^. posts) =
  [WL.renderList drawTopic True (tui ^. topics) <=> helpBar Nothing]
    where
        drawTopic selected (Topic _ category' title lastUpdated likeCount postsCount posters pinned)
                        = border
                        . (if pinned then withAttr "pinned" else id)
                        . padRight Max
                        $ (likes' <+> title' <+> lastMod) <=>
                           hBox [category, postsCount', posters']
            where
                lastMod = padLeft Max
                          . padRight (Pad 1)
                          $ txt (showTimeDelta (tui ^. currentTime) lastUpdated)

                likes' :: Widget ResourceName
                likes' = (if selected then  withAttr "selected" else id)
                         . padRight (Pad 1)
                         . hLimit 4
                         . padRight Max
                         . txt
                         . T.pack
                         . show
                         $ likeCount

                title' :: Widget ResourceName
                title' = withAttr "title" . txt $ title

                postsCount' :: Widget ResourceName
                postsCount' = padLeft (Pad 5)
                            . txt
                            . ("posts: " <>)
                            . T.pack
                            . show
                            $ postsCount

                posters' :: Widget ResourceName
                posters' = padLeft (Pad 5)
                       . hBox
                       . mapFst (withAttr "OP") (withAttr "rest")
                       . showItems
                       $ posters

                category :: Widget ResourceName
                category = padLeft (Pad 5) . txt $ category'

                -- this could perhaps be re-worked now using Vector
                showItems :: V.Vector ResourceName -> [Widget ResourceName]
                showItems v = map txt . V.toList $ (V.map (<> " ") . V.init $ v) V.++ V.singleton (V.last v)

-- We have checked for the topic list, so we are now
-- showing a topic. The issue is whether we are showing a single
-- post or not.
--
drawTui tui | tui ^. singlePostView =
  [showSelectedPost (tui ^. currentTime) (tui ^. timeOrder) posts'']
  where
    Just allPosts = view _3 <$> tui ^. posts  -- must be a Just
    order = tui ^. timeOrder

    -- We need to invert the count if order is Descending.
    --
    posts' = WL.listReverse allPosts
    posts'' = case order of
      Increasing -> allPosts
      Decreasing -> case WL.listSelected allPosts of
        Just cPos -> WL.listMoveTo cPos posts'
        Nothing -> posts'

-- Special case the single-list case so that we jump to display
-- the post.
--
drawTui tui | (listLength . view _3 <$> tui ^. posts) == Just 1
  = let ntui = tui & singlePostView .~ True
    in drawTui ntui

drawTui tui
    = [WL.renderList drawPost True posts'
       <=> helpBar (Just order)]
    where
        Just allPosts = view _3 <$> tui ^. posts
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
                border' = border
                        . vLimit 8
                        . padBottom Max
                        . padRight  Max

-- The post number and the score
postIdentifier :: Post -> T.Text
postIdentifier thisPost =
  showInt (thisPost ^. postNumber) <> ":" <> showInt (thisPost ^. likes)

showSelectedPost :: UTCTime -> TimeOrder -> WL.List n Post -> Widget ResourceName
showSelectedPost tNow order allPosts =
  case WL.listSelectedElement allPosts of
    (Just (ctr, thisPost)) ->
      let topBar = txt (postIdentifier thisPost <>
                        " " <> thisPost ^. opUserName)
                   <+> padLeft Max (txt (showTimeDelta tNow (thisPost ^. opCreatedAt)))

          nPosts = listLength allPosts

      in withAttr "OP" topBar
         <=> padBottom Max (txt $ thisPost ^. contents)
         <=> helpPostBar order (ctr + 1) nPosts

    Nothing -> txt "something went wrong"


showInt :: Int -> T.Text
showInt = T.pack . show

showTimeDelta ::
  UTCTime
  -- ^ The current time
  -> UTCTime
  -- ^ A time in the past
  -> ResourceName
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


handleTuiEvent :: TuiState -> BrickEvent ResourceName e -> EventM ResourceName (Next TuiState)
handleTuiEvent tui (VtyEvent (EvKey (KChar 'q') _)) = halt tui

-- We only change the setting when the posts are listed, not
-- in all cases.
-- handleTuiEvent tui (VtyEvent (EvKey (KChar 's') _)) =
handleTuiEvent tui (VtyEvent (EvKey (KChar 's') _)) | isJust (tui ^. posts) && not (tui ^. singlePostView) =
  let new = case tui ^. timeOrder of
              Decreasing -> Increasing
              Increasing -> Decreasing

      ntui = tui & timeOrder .~ new

  in continue ntui

-- drop the s event in all other cases
handleTuiEvent tui (VtyEvent (EvKey (KChar 's') _)) = continue tui

-- The v keypress will load the URL in a web browser (Linux only,
-- and only those that support the gio command):
--
-- topics, posts = Nothing, singlePostView = _ -> /latest
-- topics, posts = Just _, singlePostView is False -> /t/slug/id
-- topics, posts = Just _, singlePostView is True -> /t/slug/id/number
--
handleTuiEvent tui (VtyEvent (EvKey (KChar 'v') _)) = do
  let frag = case tui ^. posts of
               Just (pid, pslug, plist) ->
                 let n = case WL.listSelectedElement plist of
                       Just (_, post) -> post ^. postNumber
                       Nothing -> 1

                 in "t/" <> pslug <> "/" <> showInt pid <>
                    if tui ^. singlePostView then "/" <> showInt n else ""
               Nothing -> "latest"

  liftIO (showPage (tui ^. baseURL) frag)
  continue tui

-- up/down key presses on the single-page view will scroll through the
-- replies. up maps to previous, down to next whatever the search order.
--
handleTuiEvent tui (VtyEvent (EvKey k _)) | tui ^. singlePostView && k `elem` [KUp, KDown]
  = let Just posts' = tui ^. posts

        -- If we didn't want to make up always go bavkwards in time
        -- then we could check on k.
        step = case (k, tui ^. timeOrder) of
          (KUp, Increasing) -> -1
          (KDown, Decreasing) -> -1
          _ -> 1

        nlist = WL.listMoveBy step (posts' ^. _3)

        ntui = tui & posts ?~ (posts' & _3 .~ nlist)

    in continue ntui

handleTuiEvent tui (VtyEvent (EvKey KRight _)) | isJust (tui ^. posts)
  = do
  ntui <- liftIO (updateTime tui)
  continue $ ntui & singlePostView .~ not (ntui ^. singlePostView)

handleTuiEvent tui (VtyEvent (EvKey _ _)) | isJust (tui ^. posts) && (tui ^. singlePostView)
  = do
  ntui <- liftIO (updateTime tui)
  continue $ ntui & singlePostView .~ False

handleTuiEvent tui (VtyEvent (EvKey KRight  _)) = do
  ntui <- liftIO (updateTime tui)
  posts' <- liftIO $ getPosts ntui
  continue $ ntui & posts ?~ posts'

handleTuiEvent tui (VtyEvent (EvKey KLeft   _))
    = do
  ntui <- liftIO (updateTime tui)
  continue $ ntui & posts .~ Nothing

-- Should we update the time in these cases? It would make the display
-- "reactive", but it also might be confusing when scrolling to see
-- the times change.
--
handleTuiEvent tui ev | isJust (tui ^. posts)
  = let Just posts' = tui ^. posts
        list = posts' ^. _3
    in scrollHandler (\x -> tui & posts ?~ (posts' & _3 .~ x)) list ev

handleTuiEvent tui ev
    = scrollHandler (\x -> tui & topics .~ x) (tui ^. topics) ev

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
