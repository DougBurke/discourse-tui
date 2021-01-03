{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types (TuiState(..)
             , currentTime
             , topics
             , posts
             , baseURL
             , timeOrder
             , Category
             , categoryId
             , categoryName
             , categories
             , ProtoTopic(..)
             , topicId
             , Post(..)
             , posterId
             , opUserName
             , opCreatedAt
             , contents
             , PostResponse(..)
             , Topic(..)
             , TopicResponse(..)
             , User
             , userId
             , userName
             , TimeOrder(..)
             , ResourceName
             , topicHeight
             ) where

import qualified Data.Vector as V

import Brick.Widgets.List (List)

import Control.Lens
import Control.Lens.TH

import Data.Aeson (FromJSON, Value(Object)
                  , (.:), (.:?), (.!=)
                  , parseJSON, withObject)
import Data.Time (UTCTime)


instance FromJSON ProtoTopic where
    parseJSON = withObject "ProtoTopic" $ \v -> do 
        topicId' <- v .: "id"
        title'  <- v .: "title"
        lastUpdated' <- v .: "last_posted_at"  -- canthis be empty or missing?
        likeCount' <- v.: "like_count"
        postsCount' <- v.: "posts_count"
        posters' <- v.: "posters"
        pinned' <- v.: "pinned"
        categoryId' <- v.: "category_id"
        return $ ProtoTopic topicId' categoryId' title' lastUpdated' likeCount' postsCount' posters' pinned'

instance FromJSON User where
    parseJSON (Object v) = User
            <$> v .: "id"
            <*> v .: "username"
            <*> v .: "name"

instance FromJSON TopicResponse where
    parseJSON = withObject "TopicResponse" $ \v -> do
           users' <- v .: "users"
           topicList' <- v .: "topic_list"
           topics' <- topicList' .: "topics"
           return $ TopicResponse users' topics'

instance FromJSON Poster where
    parseJSON (Object v) = Poster
           <$> v .: "user_id"
           <*> v .: "description"

instance FromJSON Category where
    parseJSON (Object v) = Category
        <$> v .: "id"
        <*> v .: "name"
        <*> v .:? "subcategory_ids" .!= V.empty

-- We hack categories to support the "sub-categories"
instance FromJSON CategoryResponse where
    parseJSON = withObject "CategoryResponse" $ \v -> do
        categoryList <- v .: "category_list"
        categories' <- categoryList .: "categories"

        -- fake the sub-categories
        let extra = V.filter (not . null . _subCategoryIds) categories'
            dup c = V.map (fake c) (_subCategoryIds c)
            fake cat cid = Category cid (_categoryName cat ++ " [SUB-CATEGORY]") V.empty
            extra' = V.concatMap dup extra

        return $ CategoryResponse $ categories' V.++ extra'

instance FromJSON PostResponse where
    parseJSON = withObject "PastResponse" $ \v -> do
        postStream <- v .: "post_stream"
        posts' <- postStream .: "posts"
        return $ PostResponse posts'

instance FromJSON Post where
    parseJSON = withObject "Post" $ \v -> do
        id' <- v .: "id"
        username' <- v .: "username"
        createdAt' <- v .: "created_at"
        cooked' <- v .: "cooked"
        actions <- v .: "actions_summary"
        return $ Post id' username' createdAt' cooked' (if null actions then 0 else _count . head $ actions)

instance FromJSON Action where
    parseJSON (Object v) = Action
        <$> v .: "id"
        <*> v .: "count"

newtype CategoryResponse = CategoryResponse
    {
    _categories :: V.Vector Category
    } deriving (Show)

data Action = Action
    {
    _actionId :: Int,
    _count :: Int
    } deriving (Show)

data TopicResponse = TopicResponse
    {
    _users :: V.Vector User,
    _topicList :: V.Vector ProtoTopic
    } deriving (Show)

topicHeight :: Int
topicHeight = 4

data ProtoTopic = ProtoTopic
    {
    _topicId :: Int,
    _categoryID :: Int,
    _title :: String,
    _lastUpdatedProyo :: UTCTime,
    _likeCount :: Int,
    _postsCount :: Int,
    _posters :: V.Vector Poster,
    _pinned :: Bool
    } deriving (Show)

data Topic = Topic
    {
    _topicId :: Int,
    _category :: String,
    _title :: String,
    _lastUpdated :: UTCTime,
    _likeCount :: Int,
    _postsCount :: Int,
    _posters :: V.Vector String,
    _pinned :: Bool
    } deriving (Show)

data User = User
    {
    _userId :: Int,
    _userName :: String,
    _realName :: String
    } deriving (Show)


data Category = Category
    {
    _categoryId :: Int,
    _categoryName :: String,
    _subCategoryIds :: V.Vector Int
    } deriving (Show)

data Poster = Poster
    {
    _posterId :: Int,
    _description :: String
    } deriving (Show)

data PostResponse = PostResponse
    {
    _postList :: V.Vector Post
    } deriving (Show)

data Post = Post
    {
    _postId :: Int,
    _opUserName :: String,
    _opCreatedAt :: UTCTime,
    _contents :: String,
    _likes :: Int
    } deriving (Show)

data TimeOrder = Decreasing | Increasing
  deriving (Eq, Show)

data TuiState = TuiState
    {
      _currentTime :: UTCTime,
      _topics :: List String Topic,
      _posts :: Maybe (List String Post), -- Nothing if not in post view
      _baseURL :: String,
      _singlePostView :: Bool, -- if we're looking at the full contents of one post
      _timeOrder :: TimeOrder
    } deriving (Show)

type ResourceName = String

makeLenses ''CategoryResponse
makeLenses ''Post
makeLenses ''PostResponse
makeLenses ''Category
makeLenses ''Poster
makeLenses ''Topic
makeLenses ''TopicResponse
makeLenses ''Action
makeLenses ''User
makeLenses ''TuiState
