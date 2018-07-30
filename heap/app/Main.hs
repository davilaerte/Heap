{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Monoid ((<>))
import Data.Text
import HeapModule (swap, Heap(..), Comparator(..), Comparable(..), buildHeap, insert, extract, extracts, heapSort)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.=), (.:), object)

-- Funcoes auxiliares
getComparator :: (Comparable a) => Bool -> (a -> a -> Bool)
getComparator True = compareMax
getComparator False = compareMin

-- Tipos e instancias auxiliares
data HeapAux = HeapAux {comp :: Bool, heapLis :: [Integer]}

instance (ToJSON m) => ToJSON (Heap m) where
  toJSON (Heap comp heap) = object ["heap" .= heap]

instance FromJSON HeapAux where
    parseJSON (Object v) = HeapAux
        <$> v .: "comp"
        <*> v .: "heapLis"

accessControleHeaders = do
  addHeader "Access-Control-Allow-Origin" "http://localhost:8000"
  addHeader "Access-Control-Expose-Headers" "APP_VERSION"
  addHeader "Access-Control-Allow-Headers" "X-API-KEY, Origin, X-Requested-With, Content-Type, Accept, Access-Control-Request-Method"
  addHeader "Access-Control-Allow-Methods" "POST, GET, PUT, DELETE, PATCH"
  addHeader "Content-Type" "application/json; charset=utf-8"
  addHeader "APP_VERSION" "1.001"
        
-- rotas (endpoints)
routes :: ScottyM ()
routes = do
  options "/insert/:elem" $ do
    accessControleHeaders
  post "/insert/:elem" $ do
    elem <- param "elem"
    heapAux <- jsonData
    let heap = Heap (Comparator (getComparator (comp heapAux))) (heapLis heapAux)
    accessControleHeaders
    json (toJSON (insert heap elem))

  options "/extract" $ do
    accessControleHeaders
  post "/extract" $ do
    heapAux <- jsonData
    let heap = Heap (Comparator (getComparator (comp heapAux))) (heapLis heapAux)
    accessControleHeaders
    json (toJSON (extract heap))

  options "/extracts" $ do
    accessControleHeaders
  post "/extracts" $ do
    heapAux <- jsonData
    let heap = Heap (Comparator (getComparator (comp heapAux))) (heapLis heapAux)  
    accessControleHeaders
    json (toJSON (extracts heap))

  options "/build" $ do
    accessControleHeaders
  post "/build" $ do
    accessControleHeaders
    heapAux <- jsonData
    let comparator = Comparator (getComparator (comp heapAux))
    let heapLista = heapLis heapAux
    json (toJSON (buildHeap comparator heapLista))
  
  options "/sort" $ do
    accessControleHeaders
  post "/sort" $ do
    heapAux <- jsonData
    let heapLista = heapLis heapAux
    let comparator = Comparator (getComparator (comp heapAux))  
    accessControleHeaders
    json (toJSON (heapSort heapLista comparator))

main = do
  putStrLn "Iniciando server"
  scotty 3000 routes