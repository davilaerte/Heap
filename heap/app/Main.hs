{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Monoid ((<>))
import Data.Text
import Heap (swap)
import Data.Aeson (ToJSON, toJSON, (.=), object)

data Teste = Teste {x :: [Char], y :: Int}

instance ToJSON Teste where
  toJSON (Teste x y) = object ["x" .= x, "y" .= y]

routes :: ScottyM ()
routes = do
  get "/hello" $ do
    json (toJSON (Teste (swap ['a'..'z'] 3 14) 4))
  get "/teste/:name" $ do
    name <- param "name"
    addHeader "Access-Control-Allow-Origin" "http://localhost:8000"
    addHeader "Access-Control-Expose-Headers" "APP_VERSION"
    addHeader "Access-Control-Allow-Headers" "X-API-KEY, Origin, X-Requested-With, Content-Type, Accept, Access-Control-Request-Method"
    addHeader "Access-Control-Allow-Methods" "POST, GET, PUT, DELETE, PATCH"
    addHeader "Content-Type" "application/json; charset=utf-8"
    addHeader "APP_VERSION" "1.001"
    text ("OLA EU sou" <> name <> "!")

main = do
  putStrLn "Iniciando server"
  print (swap [1..10] 1 5)
  scotty 3000 routes