{-# LANGUAGE OverloadedStrings #-}

module Db(module Db) where

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Data.Text
import Data.Time.Clock

import Models

getConnection :: IO Connection
getConnection = open "cat-haven.db"

-- Function to initialize tables
initialize :: IO ()
initialize = do
  conn <- getConnection

  execute_ conn "CREATE TABLE IF NOT EXISTS gallery (\
    \id TEXT PRIMARY KEY,\
    \url TEXT NOT NULL,\
    \width INTEGER NOT NULL,\
    \height INTEGER NOT NULL,\
    \user TEXT NOT NULL)"

  execute_ conn "CREATE TABLE IF NOT EXISTS comments (\
    \id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \gallery_id TEXT NOT NULL,\
    \text TEXT NOT NULL,\
    \user TEXT NOT NULL,\
    \timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,\
    \FOREIGN KEY (gallery_id) REFERENCES gallery(id))"

  close conn
  putStrLn "\nTables initialized"

checkCatExists :: Text -> IO Bool
checkCatExists catId = do
  conn <- getConnection
  cats <- runBeamSqlite conn $ runSelectReturningList $ 
    select $ filter_ (\c -> _galleryId c ==. val_ catId) $ all_ (_gallery catGalleryDb)
  close conn
  return $ not $ Prelude.null cats

insertCat :: SaveCatRequest -> IO ()
insertCat req = do
  conn <- getConnection
  runBeamSqlite conn $ runInsert $ 
    insert (_gallery catGalleryDb) (insertValues [Gallery (imageId (sc_cat req)) (imageUrl (sc_cat req)) (fromIntegral (imageWidth (sc_cat req))) (fromIntegral (imageHeight (sc_cat req))) (sc_user req)])
  close conn

deleteCat :: Text -> IO ()
deleteCat catId = do
  conn <- getConnection
  runBeamSqlite conn $ runDelete $ 
    Database.Beam.delete (_comments catGalleryDb) (\c -> _commentsGalleryId c ==. val_ catId)
  runBeamSqlite conn $ runDelete $ 
    Database.Beam.delete (_gallery catGalleryDb) (\c -> _galleryId c ==. val_ catId)
  close conn

selectCats :: IO [Gallery]
selectCats = do
  conn <- getConnection
  cats <- runBeamSqlite conn $ runSelectReturningList $ 
    select $ all_ (_gallery catGalleryDb)
  close conn
  return cats

getCat :: Text -> IO Gallery
getCat catId = do
  conn <- getConnection
  cats <- runBeamSqlite conn $ runSelectReturningList $ 
    select $ filter_ (\c -> _galleryId c ==. val_ catId) $ all_ (_gallery catGalleryDb)
  close conn
  return $ Prelude.head cats

insertComment :: CommentCatRequest -> Text -> IO ()
insertComment req catId = do
  conn <- getConnection
  currentTimestamp <- getCurrentTime
  runBeamSqlite conn $ runInsert $ 
    insert (_comments catGalleryDb) (insertValues [Comments Nothing catId (cc_comment req) (cc_user req) currentTimestamp])

getCommentsForCat :: Text -> IO [Comments]
getCommentsForCat catId = do
  conn <- getConnection
  comments <- runBeamSqlite conn $ runSelectReturningList $ 
    select $ filter_ (\c -> _commentsGalleryId c ==. val_ catId) $ all_ (_comments catGalleryDb)
  close conn
  return comments