{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Monoid ((<>))
import Data.Text
import HeapModule (swap, Heap(..), Comparator(..), Comparable(..), buildHeap)
import Data.Aeson (ToJSON, toJSON, (.=), object)

data Teste = Teste {x :: [Char], y :: Int}

instance ToJSON Teste where
  toJSON (Teste x y) = object ["x" .= x, "y" .= y]

instance ToJSON (Heap m) where
  toJSON heap = object ["heap" .= (heapList heap)]

routes :: ScottyM ()
routes = do
  get "/hello" $ do
    json (toJSON (buildHeap (Comparator compareMax) [1..10]))
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