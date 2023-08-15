{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types (TuiState(..)
             , TopicList
             , CategoryList
             , currentTime
             , topicList
             , categoryList
             , categoryMap
             , baseURL
             , timeOrder
             , displayState
             , Category
             , categoryId
             , subCategoryList  -- note: naming is not great
             -- , subcategoryId
             , subcategoryName
             , subcategoryDesc
             , categoryName
             , categorySlug
             , categoryDesc
             , categoryNTopic
             , categoryNPost
             , categories
             , CategoryResponse
             , convertSubCat
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
             , postTitle
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
             , ExtraDownload
             , toExtraDownload
             , edTime
             , edBaseUrl
             , edRunning
             , edToDo
             , edQuery
             , edTID
             , SingleTopic
             , toSingleTopic
             , stId
             , stNumPosts
             , stSlug
             , stTitle
             , stList
             , stDownload
             , TimeOrder(..)
             , ResourceName(..)
             , Slug
             , topicHeight
             , DisplayState(..)
             , Display(..)
             , initDisplayState
             ) where

import qualified Data.IntMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import Brick.Widgets.List (List)

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Lens

import Data.Aeson (FromJSON, (.:), (.:?), (.!=)
                  , parseJSON, withObject)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)


-- Labels for the UI
--
data ResourceName = Contents | Categories | Posts | SinglePostView
  deriving (Eq, Ord, Show)


instance FromJSON ProtoTopic where
    parseJSON = withObject "ProtoTopic" $ \v -> do 
        topicId' <- v .: "id"
        title'  <- v .: "title"
        lastUpdated' <- v .:? "last_posted_at"  -- can this be empty or missing?  - can be empty
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
    parseJSON = withObject "Category" $ \v -> do
      Category
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "slug"
      <*> v .: "topic_count"
      <*> v .: "post_count"
      <*> v .:? "subcategory_list" .!= V.empty
      <*> v .: "description_text"


-- This is a bit annoying
--
instance FromJSON SubCategory where
    parseJSON = withObject "Category" $ \v ->
      SubCategory
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "slug"
      <*> v .: "topic_count"
      <*> v .: "post_count"
      <*> v .:? "description_text"

-- If asking for categories.json then we have subcategory_ids as a list
-- of integers.
--
-- If parsing categories.json?include_categories=true then we also have
-- subcategory_list as a list of categories
--
instance FromJSON CategoryResponse where
    parseJSON = withObject "CategoryResponse" $ \v -> do
        categoryList <- v .: "category_list"
        categories' <- categoryList .: "categories"
        pure $ CategoryResponse categories'


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
        title' <- v .: "title"
        posts' <- postStream .: "posts"
        postids <- postStream .: "stream"
        pure $ PostResponse chunkSize id' slug' title' posts' postids

instance FromJSON PostSelectedResponse where
    parseJSON = withObject "PostSelectedResponse" $ \v -> do
        postStream <- v .: "post_stream"
        posts' <- postStream .: "posts"
        pure $ PostSelectedResponse posts'

-- There are some posts which do things like mark the topic as pinned,
-- which have no informational content. Should we just skip them? For
-- now we include the action_code label (potentially in a "nice"
-- form).
--
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

        let out txt = Post id' postNumber' username' createdAt' txt count

        -- If this is just a "pinned globally" message then would like to skip it but
        -- let's just encode the action.
        --
        maction_code <- v .:? "action_code"
        case maction_code of
          Just action_code -> let lbl = makeActionLabel action_code
                              in if T.null cooked'
                                 then pure (out lbl)
                                 else pure (out (cooked' <> "\n\n" <> lbl))
          _ -> pure (out cooked')


-- Do not know all the actions that can be done, so treat them manually
-- as we come across them.
--
makeActionLabel :: T.Text -> T.Text
makeActionLabel "pinned_globally.enabled" = "Pinned globally"
makeActionLabel "closed.enabled" = "Closed"
makeActionLabel action = "Action: " <> action


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
    _pLastUpdated :: Maybe UTCTime,
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
    _lastUpdated :: Maybe UTCTime,
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
    _categorySlug :: T.Text,
    _categoryNTopic :: Int,
    _categoryNPost :: Int,
    _subCategoryList :: V.Vector SubCategory,
    _categoryDesc :: T.Text
    } deriving (Show)

-- Should just be able to use category but at least this
-- means we don't have to bother with sub-sub categories.
--
data SubCategory = SubCategory
    {
    _subcategoryId :: Int,
    _subcategoryName :: T.Text,
    _subcategorySlug :: T.Text,
    _subcategoryNTopic :: Int,
    _subcategoryNPost :: Int,
    _subcategoryDesc :: Maybe T.Text
    } deriving (Show)


convertSubCat :: SubCategory -> Category
convertSubCat (SubCategory a b c d e f) = Category a b c d e V.empty (fromMaybe "<sub category>" f)


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
  , _postSlug :: Slug
  , _postTitle :: T.Text
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


data ExtraDownload = ED
  { _edTime :: UTCTime  -- time the query started, approximately
  , _edBaseUrl :: String -- the URL to query (once the list of posts are attached)
  , _edRunning :: Int   -- the number of posts being queried
  , _edToDo :: V.Vector Int
  , _edQuery :: MVar (V.Vector Post)
  , _edTID :: ThreadId
  }

toExtraDownload ::
  UTCTime
  -> String
  -> Int
  -> V.Vector Int
  -> MVar (V.Vector Post)
  -> ThreadId
  -> ExtraDownload
toExtraDownload = ED

data SingleTopic = SingleTopic
  {
    _stId :: Int
  , _stNumPosts :: Int  -- the number of posts
  , _stSlug :: Slug
  , _stTitle :: T.Text
  , _stList :: List ResourceName Post
  , _stDownload :: Maybe ExtraDownload
  } -- deriving (Show)

toSingleTopic ::
  Int    -- post id
  -> Int -- the number of posts
  -> Slug
  -> T.Text -- the title
  -> List ResourceName Post
  -> Maybe ExtraDownload
  -> SingleTopic
toSingleTopic = SingleTopic

type TopicList = List ResourceName Topic
type CategoryList = List ResourceName Category

{-

Originally there was a field defining what should be displayed, but I
decided to make it so that the state defined the display, to try and
make unrepresentable states impossible, and avoid a number of
incomplete-pattern matchig checks I knew were "impossible".

-}

data TuiState =
  TuiState
  { _currentTime :: UTCTime
  , _topicList :: TopicList
  , _categoryList :: CategoryList
  , _categoryMap :: M.IntMap Category
  , _baseURL :: String
  , _timeOrder :: TimeOrder
  , _displayState :: DisplayState
  }


-- We want to be able to bounce into help and then back to the previous
-- page so we store that case as
--    DSHelp previous
-- otherwise we expect
--    DS x where x is not DisplayHelp
--
data DisplayState =
  DS Display
  | DSHelp Display

data Display =
  DisplayAllTopics
  | DisplayTopic SingleTopic
  | DisplayPost SingleTopic
  | DisplayAllCategories
  | DisplayCategory Slug Int T.Text TopicList
  | DisplayCategoryTopic Slug Int T.Text TopicList SingleTopic
  | DisplayCategoryPost Slug Int T.Text TopicList SingleTopic
  -- deriving Eq


initDisplayState :: DisplayState
initDisplayState = DS DisplayAllTopics

type Slug = T.Text

makeLenses ''CategoryResponse
makeLenses ''Post
makeLenses ''PostResponse
makeLenses ''PostSelectedResponse
makeLenses ''Category
makeLenses ''SubCategory
makeLenses ''Poster
makeLenses ''ProtoTopic
makeLenses ''Topic
makeLenses ''TopicResponse
-- makeLenses ''Action
makeLenses ''User
makeLenses ''ExtraDownload
makeLenses ''SingleTopic
makeLenses ''TuiState
