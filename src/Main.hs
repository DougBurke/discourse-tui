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
import Control.Monad (forM_, void)

import Data.Maybe (fromMaybe)
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
parseTopic userMap catagoryMap (ProtoTopic topicId catId title lastUpdated likeC postsC posters pinned)
    = Topic {
        _title = title,
        _topicId = topicId,
        _category = (catagoryMap M.! catId) ^. categoryName,
        _lastUpdated = lastUpdated,
        _likeCount = likeC,
        _postsCount = postsC,
        _posters = V.map (\x -> (userMap M.! (x ^. posterId)) ^. userName) posters,
        _pinned = pinned
            }

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
helpBar :: Maybe TimeOrder -> Widget String
helpBar mOrder = withAttr "bar" widget
  where
    widget = case mOrder of
      Just order -> str msgFull <+> dirMsg order
      Nothing -> str msgBase

    msgBase = "arrow keys -> move | left right -> read replies/full post | q to quit"
    msgFull = "arrow keys -> move | left right -> read replies/full post | s swap order | q to quit"
    dir Increasing = "↑" -- unicode 2191  "+"
    dir Decreasing = "↓" -- unicode 2193  "-"

    dirMsg order = padLeft Max
             $ str (dir order)

-- get the posts for the current topic
getPosts :: TuiState -> IO (WL.List String Post)
getPosts ts = do
    let (Just selectedTopicID) = view (_2 . topicId) <$> WL.listSelectedElement (ts ^. topics)
    postsRequest <- parseRequest $ mconcat [ts ^. baseURL, "/t/", show selectedTopicID, ".json"]
    (PostResponse posts') <- getResponseBody <$> httpJSON postsRequest
    posts <- mapM postToPandoc posts'
    pure $ WL.list "posts" posts 10

postToPandoc :: Post -> IO Post
postToPandoc post = do
    newContents <- toMarkdown $ post ^. contents
    pure $ post & contents .~ newContents

toMarkdown :: String -> IO String
toMarkdown s = do
    result <- runIO $ do
        doc <- readHtml def (T.pack s)
        writeCommonMark def doc
    rst <- handleError result
    pure $ T.unpack rst


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
                        $ (likes <+> title' <+> lastMod) <=>
                           hBox [category, postsCount', posters']
            where
                lastMod = padLeft Max
                          . padRight (Pad 1)
                          $ txt (showTimeDelta tNow lastUpdated)

                likes :: Widget ResourceName
                likes = (if selected then  withAttr "selected" else id)
                      . padRight (Pad 1)
                      . hLimit 4
                      . padRight Max
                      . str
                      . show
                      $ likeCount

                title' :: Widget ResourceName
                title' = withAttr "title" . str $ title

                postsCount' :: Widget ResourceName
                postsCount' = padLeft (Pad 5)
                            . str
                            . ("posts: " ++)
                            . show
                            $ postsCount

                posters' :: Widget ResourceName
                posters' = padLeft (Pad 5)
                       . hBox
                       . mapFst (withAttr "OP") (withAttr "rest")
                       . showList
                       $ posters

                category :: Widget ResourceName
                category = padLeft (Pad 5) . str $ category'

                -- this could perhaps be re-worked now using Vector
                showList :: V.Vector String -> [Widget ResourceName]
                showList v = map str . V.toList $ (V.map (++ " ") . V.init $ v) V.++ V.singleton (V.last v)

-- this pattern matches the post list
drawTui (TuiState tNow _ (Just posts) _ False order)
    = [WL.renderList drawPost True posts'
       <=> helpBar (Just order)]
    where
        posts' = orderSelect order posts

        drawPost selected (Post id username' createdAt' contents score')
            = border'
            $ withAttr (if selected then "selected" else "")
              (hLimit 4 . padRight Max . str . show $ score') 
            <+> ((userName'' <+> created)
                 <=> contents')
            where
                userName'' = withAttr "OP" . str $ username'
                created = withAttr "title"
                          . padLeft Max
                          . padRight (Pad 1)
                          $ txt (showTimeDelta tNow createdAt')
                contents' = strWrap contents
                border' = border
                        . vLimit 8
                        . padBottom Max
                        . padRight  Max

drawTui (TuiState tNow _ (Just posts) _ True order) =
  [elem]
  where
    -- We need to invert the count if order is Descending.
    --
    posts' = WL.listReverse posts
    posts'' = case order of
      Increasing -> posts
      Decreasing -> case WL.listSelected posts of
        Just cPos -> WL.listMoveTo cPos posts'
        Nothing -> posts'

    elem = case WL.listSelectedElement posts'' of
      (Just (_, post)) -> withAttr "OP" (str (post ^. opUserName)
                                         <+> padLeft Max (txt (showTimeDelta tNow (post ^. opCreatedAt)))
                                        )
                          <=> padBottom Max (str $ post ^. contents)
                          <=> helpBar Nothing
      Nothing -> txt "something went wrong"


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

handleTuiEvent :: TuiState -> BrickEvent String e -> EventM String (Next TuiState)
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

handleTuiEvent (TuiState _ topics (Just list) url singlePostView order) (VtyEvent (EvKey KRight _))
    = do
  now <- liftIO getCurrentTime
  continue $ TuiState now topics (Just list) url (not singlePostView) order

handleTuiEvent (TuiState _ topics (Just list) url True order) (VtyEvent (EvKey _ _))
    = do
  now <- liftIO getCurrentTime
  continue $ TuiState now topics (Just list) url False order

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

handleTuiEvent (TuiState now topics (Just list) url spv order) ev
    = scrollHandler (\x -> TuiState now topics (Just x) url spv order) list ev

scrollHandler restoreTuiState list (VtyEvent ev) = continue . restoreTuiState =<< handler
    where
        handler = WL.handleListEvent ev list
