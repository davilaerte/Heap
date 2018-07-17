swap :: [a] -> Int -> Int -> [a]
swap lst i j = swapAux lst 0
    where swapAux [] _ = []
          swapAux (a:as) index | index == i = lst !! j : swapAux as (index+1)
                               | index == j = lst !! i : swapAux as (index+1)
                               | otherwise = a : swapAux as (index+1)

data MinHeap a = MinHeap [a] deriving (Eq, Show)

class Heap a where
    buildHeap :: a -> a
    heapFy :: int -> a -> a
    extract :: a -> b

-- data MinHeap a = MinHeap [a] deriving (Eq, Show)

-- instance Heap (MinHeap a) where
--     buildHeap (MinHeap x) = MinHeap [y | y <- x, False]
--     heapFy x heap = heap
--     extract (MinHeap x) = 1