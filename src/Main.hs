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

import Control.Lens ((&), (?~), (.~), (^.), _2
                    , view)
import Control.Monad (void)

import Data.Time (UTCTime, diffUTCTime, getCurrentTime)

import Graphics.Vty.Input.Events (Event(..), Key(..))
import Graphics.Vty.Attributes (blue, bold, defAttr, green, withStyle, yellow)

import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)

import System.Environment (getArgs)
import System.Exit (die)

import Text.Pandoc (runIO, def, handleError, readHtml, writeCommonMark)

import Types

-- Change a protoTopic into a topic by consulting userMap and catagoryMap.
-- Unfortunately we can not guarantee that all categories are recorded in
-- the categoryMap (at least the Haskell discourse doesn't seem to
-- return all items). Actually, maybe it's the "subcategory" support in
-- the categories endpoint which is less-than-ideal.
--
parseTopic :: M.IntMap User -> M.IntMap Category -> ProtoTopic -> Topic
parseTopic userMap catagoryMap (ProtoTopic topicId' catId title lastUpdated likeC postsC posters pinned)
    = Topic {
        _title = title,
        _topicId = topicId',
        _category = (catagoryMap M.! catId) ^. categoryName,
        _lastUpdated = lastUpdated,
        _likeCount = likeC,
        _postsCount = postsC,
        _posters = V.map (\x -> (userMap M.! (x ^. posterId)) ^. userName) posters,
        _pinned = pinned
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
getTuiState baseUrl = do
    topicsRequest <- parseRequest (baseUrl ++ "/latest.json")
    categoriesRequest <- parseRequest (baseUrl ++ "/categories.json")
    categoriesResp <- getResponseBody <$> httpJSON categoriesRequest
    (TopicResponse users topicList) <- getResponseBody <$> httpJSON topicsRequest
    let userMap     = M.fromList . V.toList . V.map (\x -> (x ^. userId, x)) $ users
    let categoryMap = M.fromList . V.toList . V.map (\x -> (x ^. categoryId, x)) $ categoriesResp ^. categories
    now <- getCurrentTime
    pure TuiState {
      _currentTime = now,
      _posts = Nothing,
      _topics = WL.list "contents" (V.map (parseTopic userMap categoryMap) topicList) topicHeight,
      _baseURL = baseUrl,
      _singlePostView = False,
      _timeOrder = Increasing
      }

-- the help bar at the bottom
helpBar :: Maybe TimeOrder -> Widget ResourceName
helpBar mOrder = withAttr "bar" widget
  where
    widget = case mOrder of
      Just order -> txt msgFull <+> dirMsg order
      Nothing -> txt msgBase

    msgBase = "arrow keys -> move | left right -> read replies/full post | q to quit"
    msgFull = "arrow keys -> move | left right -> read replies/full post | s swap order | q to quit"
    dir Increasing = "↑" -- unicode 2191  "+"
    dir Decreasing = "↓" -- unicode 2193  "-"

    dirMsg order = padLeft Max
             $ txt (dir order)

-- get the posts for the current topic
--
-- TODO: pagination, but for now just use ?print=true which is meant to get
--       up to 1000 posts, which should be good for me (as I can't see an obvious
--       pagination feature). Hmm, this seems to trigger user-access restrictions!
--
getPosts :: TuiState -> IO (WL.List ResourceName Post)
getPosts ts = do
    let Just selectedTopicID = view (_2 . topicId) <$> WL.listSelectedElement (ts ^. topics)
        postURL = mconcat [ts ^. baseURL, "/t/", show selectedTopicID, ".json" {- , "?print=true" -}]

    postsRequest <- parseRequest postURL
    (PostResponse _ maxCount posts') <- getResponseBody <$> httpJSON postsRequest
    allPosts <- mapM postToPandoc posts'
    pure $ WL.list "posts" allPosts 10

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
drawTui (TuiState tNow scrollable Nothing _ _ _) =
  [WL.renderList drawTopic True scrollable <=> helpBar Nothing]
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
                          $ txt (showTimeDelta tNow lastUpdated)

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

-- this pattern matches the post list
drawTui (TuiState tNow _ (Just allPosts) _ False order)
    = [WL.renderList drawPost True posts'
       <=> helpBar (Just order)]
    where
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
                          $ txt (showTimeDelta tNow (post ^. opCreatedAt))
                contents' = txtWrap (post ^. contents)
                border' = border
                        . vLimit 8
                        . padBottom Max
                        . padRight  Max

drawTui (TuiState tNow _ (Just allPosts) _ True order) =
  [showSelectedPost tNow posts'']
  where
    -- We need to invert the count if order is Descending.
    --
    posts' = WL.listReverse allPosts
    posts'' = case order of
      Increasing -> allPosts
      Decreasing -> case WL.listSelected allPosts of
        Just cPos -> WL.listMoveTo cPos posts'
        Nothing -> posts'

-- The post number and the score
postIdentifier :: Post -> T.Text
postIdentifier thisPost =
  showInt (thisPost ^. postNumber) <> ":" <> showInt (thisPost ^. likes)

showSelectedPost :: UTCTime -> WL.List n Post -> Widget ResourceName
showSelectedPost tNow allPosts =
  case WL.listSelectedElement allPosts of
    (Just (_, thisPost)) ->
      let topBar = txt (postIdentifier thisPost <>
                        " " <> thisPost ^. opUserName)
                   <+> padLeft Max (txt (showTimeDelta tNow (thisPost ^. opCreatedAt)))

      in withAttr "OP" topBar
         <=> padBottom Max (txt $ thisPost ^. contents)
         <=> helpBar Nothing

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


handleTuiEvent :: TuiState -> BrickEvent ResourceName e -> EventM ResourceName (Next TuiState)
handleTuiEvent tui (VtyEvent (EvKey (KChar 'q') _)) = halt tui

-- We only change the setting when the posts are listed, not
-- in all cases.
-- handleTuiEvent tui (VtyEvent (EvKey (KChar 's') _)) =
handleTuiEvent tui@(TuiState _ _ (Just _) _ False order) (VtyEvent (EvKey (KChar 's') _)) =
  let new = case order of
              Decreasing -> Increasing
              Increasing -> Decreasing

      ntui = tui & timeOrder .~ new

  in continue ntui

-- drop the s event in all other cases
handleTuiEvent tui (VtyEvent (EvKey (KChar 's') _)) = continue tui

handleTuiEvent (TuiState _ topicList (Just list) url singlePostView order) (VtyEvent (EvKey KRight _))
    = do
  now <- liftIO getCurrentTime
  continue $ TuiState now topicList (Just list) url (not singlePostView) order

handleTuiEvent (TuiState _ topicList (Just list) url True order) (VtyEvent (EvKey _ _))
    = do
  now <- liftIO getCurrentTime
  continue $ TuiState now topicList (Just list) url False order

handleTuiEvent tui (VtyEvent (EvKey KRight  _)) = do
  now <- liftIO getCurrentTime
  posts' <- liftIO $ getPosts tui
  continue $ tui & currentTime .~ now & posts ?~ posts'

handleTuiEvent tui (VtyEvent (EvKey KLeft   _))
    = do
  now <- liftIO getCurrentTime
  continue $ tui & currentTime .~ now & posts .~ Nothing

-- should we update the time in these cases?
handleTuiEvent (TuiState now list Nothing url spv order) ev
    = scrollHandler (\x -> TuiState now x Nothing url spv order) list ev

handleTuiEvent (TuiState now topicList (Just list) url spv order) ev
    = scrollHandler (\x -> TuiState now topicList (Just x) url spv order) list ev

-- This is not exhaustive in BrickEvent
scrollHandler ::
  Ord n
  => (WL.List n e -> s)
  -> WL.List n e
  -> BrickEvent m f
  -> EventM n (Next s)
scrollHandler restoreTuiState list (VtyEvent ev) = continue . restoreTuiState =<< handler
    where
        handler = WL.handleListEvent ev list
-- scrollHandler restoreTuiState _ _ = continue . restoreTuiState  -- is this correct?
