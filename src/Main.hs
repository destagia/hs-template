{-# LANGUAGE OverloadedStrings #-}
module Main where

import Server
import API

main :: IO ()
main = do
  ctx <- newContext
  cscotty 3000 ctx api
