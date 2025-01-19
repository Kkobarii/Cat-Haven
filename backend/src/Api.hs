{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Api(module Api) where

import Network.HTTP.Conduit (simpleHttp)
import Control.Monad.IO.Class (liftIO, MonadIO (liftIO))
import Data.Aeson (decode)
import Web.Scotty (ActionM, json, jsonData, param)
import Data.Text (Text, unpack)

import Models
import Db

-- Endpoint: GET /random-cat
getRandomCat :: ActionM ()
getRandomCat = do
  liftIO $ putStrLn "\nFetching random cat picture..."

  response <- liftIO $ simpleHttp "https://api.thecatapi.com/v1/images/search"
  let decodedResponse = decode response :: Maybe [CatImage]

  case decodedResponse of
    Just (c:_) -> do
      json c
    Nothing -> do
      json ("error" :: String)

-- Endpoint: POST /save-cat
saveCat :: ActionM ()
saveCat = do
  req <- jsonData :: ActionM SaveCatRequest
  liftIO $ putStrLn $ "\nSaving cat " ++ show (imageId (sc_cat req)) ++ " for user " ++ unpack (sc_user req)

  exists <- liftIO $ Db.checkCatExists (imageId (sc_cat req))

  (if exists then (do
    liftIO $ putStrLn "Cat already exists"
    json ("Cat already exists" :: String)

    ) else (do
    liftIO $ Db.insertCat req
    liftIO $ putStrLn "Cat saved successfully!"
    json ("Cat saved successfully!" :: String)
    ))
    
-- Endpoint: GET /cats
getCats :: ActionM ()
getCats = do
  liftIO $ putStrLn "\nFetching all cats..."

  cats <- liftIO Db.selectCats
  liftIO $ putStrLn $ "Cats fetched successfully!"
  json cats

-- Endpoint: DELETE /delete-cat/:id
deleteCat :: ActionM ()
deleteCat = do
  catId <- param @Text "id"
  req <- jsonData :: ActionM DeleteCatRequest
  liftIO $ putStrLn $ "\nDeleting cat with ID: " ++ unpack catId

  exists <- liftIO $ Db.checkCatExists catId

  (if exists then (do
    cat <- liftIO $ Db.getCat catId
    (if dc_user req == _galleryUser cat then (do

        liftIO $ Db.deleteCat catId
        liftIO $ putStrLn "Cat deleted successfully!"
        json ("Cat deleted successfully!" :: String)

        ) else (do
            liftIO $ putStrLn "User not authorized to delete this cat"
            json ("User not authorized to delete this cat" :: String)
        ))

    ) else (do
    liftIO $ putStrLn "Cat not found"
    json ("Cat not found" :: String)
    ))

-- Endpoint: PUT /comment-cat/:id
commentCat :: ActionM ()
commentCat = do
  catId <- param @Text "id"
  req <- jsonData :: ActionM CommentCatRequest
  liftIO $ putStrLn $ "\nCommenting on cat with ID: " ++ unpack catId

  exists <- liftIO $ Db.checkCatExists catId

  (if exists then (do
    liftIO $ Db.insertComment req catId
    liftIO $ putStrLn "Comment added successfully!"
    json ("Comment added successfully!" :: String)

    ) else (do
    liftIO $ putStrLn "Cat not found"
    json ("Cat not found" :: String)
    ))

-- Endpoint: GET /comments/:id
getCommentsForCat :: ActionM ()
getCommentsForCat = do
  catId <- param @Text "id"
  liftIO $ putStrLn $ "\nFetching comments for cat with ID: " ++ unpack catId

  exists <- liftIO $ Db.checkCatExists catId

  (if exists then (do
    comments <- liftIO $ Db.getCommentsForCat catId
    liftIO $ putStrLn "Comments fetched successfully!"
    json comments

    ) else (do
    liftIO $ putStrLn "Cat not found"
    json ("Cat not found" :: String)
    ))
