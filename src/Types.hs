{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types (TuiState(..)
             , currentTime
             , showHelp
             , topics
             , posts
             , baseURL
             , singlePostView
             , timeOrder
             , Category
             , categoryId
             , categoryName
             , categories
             , ProtoTopic
             , pTopicId
             , pCategoryId
             , pTitle
             , pLastUpdated
             , pLikeCount
             , pPostsCount
             , pPosters
             , pPinned
             , Post(..)
             , topicId
             , posterId
             , postId
             , postNumber
             , opUserName
             , opCreatedAt
             , contents
             , likes
             , PostResponse
             , postChunkSize
             -- , postHighest
             , postResponseId
             , postSlug
             , postList
             , postIds
             , PostSelectedResponse
             , postSelList
             , Topic(..)
             , category
             , title
             , lastUpdated
             , likeCount
             , postsCount
             , posters
             , pinned
             , TopicResponse
             , tpUsers
             , tpTopicList
             , User
             , userId
             , userName
             , TimeOrder(..)
             , ResourceName
             , Slug
             , topicHeight
             ) where

import qualified Data.Text as T
import qualified Data.Vector as V

import Brick.Widgets.List (List)

import Control.Lens

import Data.Aeson (FromJSON, (.:), (.:?), (.!=)
                  , parseJSON, withObject)
import Data.Time (UTCTime)


instance FromJSON ProtoTopic where
    parseJSON = withObject "ProtoTopic" $ \v -> do 
        topicId' <- v .: "id"
        title'  <- v .: "title"
        lastUpdated' <- v .: "last_posted_at"  -- can this be empty or missing?
        likeCount' <- v.: "like_count"
        postsCount' <- v.: "posts_count"
        posters' <- v.: "posters"
        pinned' <- v.: "pinned"
        categoryId' <- v.: "category_id"
        pure $ ProtoTopic topicId' categoryId' title' lastUpdated' likeCount' postsCount' posters' pinned'

-- TODO: should we replace name by username when name == ""?
-- Also, on the  nix discourse there's at least one record
-- where name is null ("name": ,)
--
instance FromJSON User where
    parseJSON = withObject "User" $ \v ->
      User
      <$> v .: "id"
      <*> v .: "username"
      <*> v .:? "name" .!= ""

instance FromJSON TopicResponse where
    parseJSON = withObject "TopicResponse" $ \v -> do
           users' <- v .: "users"
           topicList' <- v .: "topic_list"
           topics' <- topicList' .: "topics"
           pure $ TopicResponse users' topics'

instance FromJSON Poster where
    parseJSON = withObject "Poster" $ \v ->
      Poster
      <$> v .: "user_id"
      <*> v .: "description"

instance FromJSON Category where
    parseJSON = withObject "Category" $ \v ->
      Category
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
            fake cat cid = Category cid (_categoryName cat <> " [SUB-CATEGORY]") V.empty
            extra' = V.concatMap dup extra

        pure $ CategoryResponse $ categories' V.++ extra'

-- post_stream.stream is a list of integers of all the items;
-- these give the id of the individual posts, so we can find the
-- missing records. How do we then grab that data?
--
instance FromJSON PostResponse where
    parseJSON = withObject "PostResponse" $ \v -> do
        postStream <- v .: "post_stream"
        chunkSize <- v .: "chunk_size"
        -- do we want highest_post_number or posts_count or .. ?
        -- highestPost <- v .: "highest_post_number"
        id' <- v .: "id"
        slug' <- v .: "slug"
        posts' <- postStream .: "posts"
        postids <- postStream .: "stream"
        pure $ PostResponse chunkSize id' slug' posts' postids

instance FromJSON PostSelectedResponse where
    parseJSON = withObject "PostSelectedResponse" $ \v -> do
        postStream <- v .: "post_stream"
        posts' <- postStream .: "posts"
        pure $ PostSelectedResponse posts'

instance FromJSON Post where
    parseJSON = withObject "Post" $ \v -> do
        id' <- v .: "id"
        postNumber' <- v .: "post_number"

        -- Pick the display_name field if not missing/empty, otherwise the username.
        -- Not the nicest bit of code.
        --
        -- Some people have the same username as display_username, so just drop
        -- the duplication.
        --
        n1 <- v .: "display_username" .!= ""
        n2 <- v .: "username"
        let username' = if n1 == "" || n2 == n1 then n2 else n1 <> " (" <> n2 <> ")"

        createdAt' <- v .: "created_at"
        cooked' <- v .: "cooked"
        actions <- v .: "actions_summary"
        let count = if null actions
                    then 0
                    else _count (V.unsafeHead actions)
        pure $ Post id' postNumber' username' createdAt' cooked' count

instance FromJSON Action where
    parseJSON = withObject "Action" $ \v ->
      Action
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
    _tpUsers :: V.Vector User,
    _tpTopicList :: V.Vector ProtoTopic
    } deriving (Show)

topicHeight :: Int
topicHeight = 4

data ProtoTopic = ProtoTopic
    {
    _pTopicId :: Int,
    _pCategoryId :: Int,
    _pTitle :: T.Text,
    _pLastUpdated :: UTCTime,
    _pLikeCount :: Int,
    _pPostsCount :: Int,
    _pPosters :: V.Vector Poster,
    _pPinned :: Bool
    } deriving (Show)

data Topic = Topic
    {
    _topicId :: Int,
    _category :: T.Text,
    _title :: T.Text,
    _lastUpdated :: UTCTime,
    _likeCount :: Int,
    _postsCount :: Int,
    _posters :: V.Vector T.Text,
    _pinned :: Bool
    } deriving (Show)

data User = User
    {
    _userId :: Int,
    _userName :: T.Text,
    _realName :: T.Text
    } deriving (Show)


data Category = Category
    {
    _categoryId :: Int,
    _categoryName :: T.Text,
    _subCategoryIds :: V.Vector Int
    } deriving (Show)

data Poster = Poster
    {
    _posterId :: Int,
    _description :: T.Text
    } deriving (Show)

data PostResponse = PostResponse
  {
    _postChunkSize :: Int
  -- , _postHighest :: Int
  , _postResponseId :: Int
  , _postSlug :: T.Text
  , _postList :: V.Vector Post
  , _postIds :: V.Vector Int
  } deriving (Show)

-- We are asking for specific posts
newtype PostSelectedResponse = PostSelectedResponse
  {
    _postSelList :: V.Vector Post
  } deriving (Show)

data Post = Post
    {
    _postId :: Int,
    _postNumber ::Int,
    _opUserName :: T.Text,
    _opCreatedAt :: UTCTime,
    _contents :: T.Text,
    _likes :: Int
    } deriving (Show)

data TimeOrder = Decreasing | Increasing
  deriving (Eq, Show)

data TuiState = TuiState
    {
      _currentTime :: UTCTime,
      _showHelp :: Bool,
      _topics :: List T.Text Topic,
      _posts :: Maybe (Int, Slug, List T.Text Post), -- Nothing if not in post view
      _baseURL :: String,
      _singlePostView :: Bool, -- if we're looking at the full contents of one post
      _timeOrder :: TimeOrder
    } deriving (Show)

type ResourceName = T.Text
type Slug = T.Text

makeLenses ''CategoryResponse
makeLenses ''Post
makeLenses ''PostResponse
makeLenses ''PostSelectedResponse
makeLenses ''Category
makeLenses ''Poster
makeLenses ''ProtoTopic
makeLenses ''Topic
makeLenses ''TopicResponse
-- makeLenses ''Action
makeLenses ''User
makeLenses ''TuiState
