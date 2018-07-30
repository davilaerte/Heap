module Heap (
    swap
) where


swap :: [a] -> Int -> Int -> [a]
swap lst i j = swapAux lst 0
    where swapAux [] _ = []
          swapAux (a:as) index | index == i = lst !! j : swapAux as (index+1)
                               | index == j = lst !! i : swapAux as (index+1)
                               | otherwise = a : swapAux as (index+1)

parent :: Int -> Int
parent x = div (x-1) 2

left :: Int -> Int
left x = (2*x) + 1

right :: Int -> Int
right x = (x + 1) * 2

class (Ord a) => Heap a where
    buildHeap :: (a -> a -> Bool) -> [a] -> [a]
    heapFy :: Int -> (a -> a -> Bool) -> [a] -> [a]
    extract :: [a] -> (a -> a -> Bool) -> (a, [a])
    insert :: (a -> a -> Bool) -> a -> [a] -> [a]

    heapFy i compare heap | i /= maior = heapFy maior compare (swap heap i heapFyAux)
                          | otherwise = heap
        where dir = right i
              esq = left i
              len = length heap
              maior = heapFyAux
              heapFyAux | esq >= len = i
                        | dir >= len  = if (heap !! esq) `compare` (heap !! i) then esq else i
                        | (heap !! esq) `compare` (heap !! i) && (heap !! esq) `compare` (heap !! dir) = esq
                        | (heap !! dir) `compare` (heap !! i) && (heap !! dir) `compare` (heap !! esq) = dir
                        | otherwise = i
    
    buildHeap compare [] = []
    buildHeap compare heap = buildAux pai heap
        where pai = parent ((length heap) - 1)
              buildAux 0 heapAux = heapFy 0 compare heapAux
              buildAux i heapAux = buildAux (i-1) (heapFy i compare heapAux)
    
    extract (a:as) compare = (a, (heapFy 0 compare ((last as):(init as))))

    insert compare element heap = insertAux (heap ++ [element]) pos
        where pos = (length (heap ++ [element]) - 1)
              insertAux heapAux 0 = heapAux
              insertAux heapAux i | (heapAux !! i) `compare` (heapAux !! (parent i)) = insertAux (swap heapAux (parent i) i) (parent i)
                                  | otherwise = heapAux 



instance Heap Integer where

instance Heap Char where
                        