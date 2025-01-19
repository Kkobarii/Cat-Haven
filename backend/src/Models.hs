{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Models(module Models) where


import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Data.Text
import Data.Int
import Data.Time.Clock
import GHC.RTS.Flags (TraceFlags(timestamp))



-- Requests and Responses
data CatImage = CatImage
  { imageId :: Text
  , imageUrl :: Text
  , imageWidth :: Int
  , imageHeight :: Int
  } deriving (Show, Generic)

instance FromJSON CatImage where
  parseJSON (Object v) = do
    CatImage <$> v .: "id"
             <*> v .: "url"
             <*> v .: "width"
             <*> v .: "height"
  parseJSON _ = fail "Expected an object"

instance ToJSON CatImage where
  toJSON (CatImage id url width height) = object
    [ "id" .= id
    , "url" .= url
    , "width" .= width
    , "height" .= height
    ]

data SaveCatRequest = SaveCatRequest
  { sc_user :: Text
  , sc_cat :: CatImage
  } deriving (Show, Generic)

instance FromJSON SaveCatRequest where
  parseJSON = withObject "SaveCatRequest" $ \v -> SaveCatRequest
    <$> v .: "user"
    <*> v .: "cat"

instance ToJSON SaveCatRequest where
  toJSON (SaveCatRequest user cat) = object
    [ "user" .= user
    , "cat" .= cat
    ]

data DeleteCatRequest = DeleteCatRequest
  { dc_user :: Text
  } deriving (Show, Generic)

instance FromJSON DeleteCatRequest where
    parseJSON = withObject "DeleteCatRequest" $ \v -> DeleteCatRequest
        <$> v .: "user"

instance ToJSON DeleteCatRequest where
    toJSON (DeleteCatRequest user) = object
        [ "user" .= user
        ]


data CommentCatRequest = CommentCatRequest
  { cc_user :: Text
  , cc_comment :: Text
  } deriving (Show, Generic)

instance FromJSON CommentCatRequest where
    parseJSON = withObject "CommentCatRequest" $ \v -> CommentCatRequest
        <$> v .: "user"
        <*> v .: "comment"

instance ToJSON CommentCatRequest where
    toJSON (CommentCatRequest user comment) = object
        [ "user" .= user
        , "comment" .= comment
        ]
        

-- Gallery table
data GalleryT f = Gallery
  { _galleryId     :: Columnar f Text
  , _galleryUrl    :: Columnar f Text
  , _galleryWidth  :: Columnar f Int32
  , _galleryHeight :: Columnar f Int32
  , _galleryUser   :: Columnar f Text
  } deriving (Generic, Beamable)

type Gallery = GalleryT Identity
deriving instance Show Gallery
deriving instance Eq Gallery

instance ToJSON Gallery where
  toJSON (Gallery id url width height user) =
    object [ "id" .= id
           , "url" .= url
           , "width" .= width
           , "height" .= height
           , "user" .= user
           ]

type GalleryId = PrimaryKey GalleryT Identity
deriving instance Show GalleryId
deriving instance Eq GalleryId

instance Table GalleryT where
  data PrimaryKey GalleryT f = GalleryId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = GalleryId . _galleryId


-- Comments table
data CommentsT f = Comments
  { _commentsId        :: Columnar f (Maybe Int32)
  , _commentsGalleryId :: Columnar f Text
  , _commentsText      :: Columnar f Text
  , _commentsUser      :: Columnar f Text
  , _commentsTimestamp :: Columnar f UTCTime
  } deriving (Generic, Beamable)

type Comments = CommentsT Identity
deriving instance Show Comments
deriving instance Eq Comments

instance ToJSON Comments where
  toJSON (Comments id galleryId text user timestamp) =
    object [ "text" .= text
           , "user" .= user
           , "timestamp" .= timestamp
           ]

type CommentsId = PrimaryKey CommentsT Identity
deriving instance Show CommentsId
deriving instance Eq CommentsId

instance Table CommentsT where
  data PrimaryKey CommentsT f = CommentsId (Columnar f (Maybe Int32))
    deriving (Generic, Beamable)
  primaryKey = CommentsId . _commentsId

-- Database
data CatGalleryDb f = CatGalleryDb
  { _gallery :: f (TableEntity GalleryT),
    _comments :: f (TableEntity CommentsT)
  } deriving (Generic, Database be)

catGalleryDb :: DatabaseSettings be CatGalleryDb
catGalleryDb = defaultDbSettings
