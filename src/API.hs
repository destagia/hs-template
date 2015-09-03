{-# LANGUAGE OverloadedStrings #-}
module API where

import qualified Data.Text.Lazy         as LT
import qualified Database.Persist       as DB
import qualified Database.Persist.MySQL as DB
import qualified Database.Persist.Sql   as DB
import           Web.Scotty.Trans

import           Model
import           Server

api :: CScottyM ()
api = do
  get "/" $ text "hs-blog v0.0.1"

  post "/article" $ do
    title <- param "title"
    bdy <- param "body"
    execDB $ DB.insert $ Article title bdy
    text "inserted"

  get "/article/:id" $ do
    aid <- param "id"
    text aid
