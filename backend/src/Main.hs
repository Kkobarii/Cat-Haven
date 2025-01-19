{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Web.Scotty (scotty, get, post, delete, middleware)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsRequestHeaders, corsMethods)
import Network.Wai (Middleware)

import Db
import Api

-- Main Application
main :: IO ()
main = do
  Db.initialize

  putStrLn "Starting Cat Haven Server..."

  scotty 3000 $ do
    middleware enableCors
    get "/cats/random" Api.getRandomCat
    post "/cats" Api.saveCat
    get "/cats" Api.getCats
    delete "/cats/:id" Api.deleteCat
    post "/cats/:id/comments" Api.commentCat
    get "/cats/:id/comments" Api.getCommentsForCat

-- Enable CORS so frontend works
enableCors :: Middleware
enableCors = cors $ const $ Just simpleCorsResourcePolicy
    { corsMethods = ["GET", "POST", "DELETE"]
    , corsRequestHeaders = ["Content-Type"]
    }
