{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Monoid ((<>))

routes :: ScottyM ()
routes = do
  get "/hello" $ do
    text "hello world!"
  get "/teste/:name" $ do
    name <- param "name"
    text ("OLA EU sou" <> name <> "!")

main = do
  putStrLn "Iniciando server"
  scotty 3000 routes