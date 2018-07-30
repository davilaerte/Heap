module HeapModule (
    swap,
    Heap(..),
    extract,
    extracts,
    buildHeap,
    heapSort,
    insert,
    Comparable(..),
    Comparator(..)
) where


-- Typeclasses e instancias auxiliares
class Comparable a where
    compareMax :: (a -> a -> Bool)
    compareMin :: (a -> a -> Bool)
    
    compareMax a b = compareMin b a
    compareMin a b = compareMax b a
    
instance Comparable Integer where
    compareMax a b = a > b

instance Comparable Char where
    compareMax a b = a > b    
    
instance (Show m) => Show (Heap m) where
    show b = show (heapList b)

-- Tipos

data Comparator a = Comparator { compareTo :: (a -> a -> Bool) }

data Heap a = Heap { comparator :: (Comparator a), heapList :: [a] }

-- Funcoes auxiliares

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

--TypeClass e instancias para Heap

class (Comparable a) => HeapI a where
    heapFy :: (Heap a) -> Int -> (Heap a)
    buildHeap :: (Comparator a) -> [a] -> (Heap a)
    insert :: (Heap a) -> a -> (Heap a)
    extract :: (Heap a) -> (a, (Heap a))
    extracts :: (Heap a) -> [a]
    heapSort :: [a] -> (Comparator a) -> [a]
    
    heapFy heap i | i /= maior = heapFy (Heap (comparator heap) (swap (heapList heap) i heapFyAux)) maior -- Poderia trocar o heapFyAux pelo maior ?
                  | otherwise = heap
        where dir = right i
              esq = left i
              len = length (heapList heap)
              maior = heapFyAux
              heapFyAux | esq >= len = i
                        | dir >= len  = if (compareTo (comparator heap)) ((heapList heap) !! esq) ((heapList heap) !! i) then esq else i
                        | (compareTo (comparator heap)) ((heapList heap) !! esq) ((heapList heap) !! i) && (compareTo (comparator heap)) ((heapList heap) !! esq) ((heapList heap) !! dir) = esq
                        | (compareTo (comparator heap)) ((heapList heap) !! dir) ((heapList heap) !! i) && (compareTo (comparator heap)) ((heapList heap) !! dir) ((heapList heap) !! esq) = dir
                        | otherwise = i
                        
    buildHeap comparator [] = Heap comparator []
    buildHeap comparator heap = Heap comparator (buildAux pai heap)
        where pai = parent ((length heap) - 1)
              buildAux i heapAux | i >= 0 = buildAux (i-1) (heapList (heapFy (Heap comparator heapAux) i))
                                 | otherwise = heapAux
    
    insert heap element = Heap (comparator heap) (insertAux ((heapList heap) ++ [element]) pos)
        where pos = (length ((heapList heap) ++ [element]) - 1)
              insertAux heapAux 0 = heapAux
              insertAux heapAux i | (compareTo (comparator heap)) (heapAux !! i) (heapAux !! (parent i)) = insertAux (swap heapAux (parent i) i) (parent i)
                                  | otherwise = heapAux
    
    extract (Heap comp (a:[])) = (a, (Heap comp []))
    extract (Heap comp (a:as)) = (a, (heapFy (Heap comp ((last as):(init as))) 0))
    
    extracts (Heap comp []) = []
    extracts (Heap comp heap) = [(fst extracted)] ++ extracts (snd extracted)
        where extracted = (extract (Heap comp heap))
    
    heapSort lis comp = extracts (buildHeap comp lis)

instance HeapI Integer where

instance HeapI Char where 