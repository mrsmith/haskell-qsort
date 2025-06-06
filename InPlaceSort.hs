module InPlaceSort (inPlaceQuickSort, isSorted) where

import Data.Array.ST
import Data.Array.MArray
import Control.Monad.ST
import Control.Monad

-- In-place quicksort using mutable arrays (matches C implementation)
inPlaceQuickSort :: (Ord a) => [a] -> [a]
inPlaceQuickSort xs = runST $ do
    let n = length xs
    arr <- newListArray (0, n-1) xs
    quickSortST arr 0 (n-1)
    getElems arr

-- ST monad quicksort with Lomuto partition (same as C implementation)
quickSortST :: (Ord a) => STArray s Int a -> Int -> Int -> ST s ()
quickSortST arr low high = do
    when (low < high) $ do
        pi <- partitionST arr low high
        quickSortST arr low (pi - 1)
        quickSortST arr (pi + 1) high

-- Lomuto partition scheme (identical logic to C version)
partitionST :: (Ord a) => STArray s Int a -> Int -> Int -> ST s Int
partitionST arr low high = do
    pivot <- readArray arr high
    let partitionLoop i j
            | j >= high = do
                -- Swap arr[i+1] and arr[high] (place pivot in correct position)
                elem_i1 <- readArray arr (i + 1)
                writeArray arr (i + 1) pivot
                writeArray arr high elem_i1
                return (i + 1)
            | otherwise = do
                elem_j <- readArray arr j
                if elem_j < pivot
                    then do
                        -- Swap arr[i+1] and arr[j] (increment i first)
                        let newI = i + 1
                        elem_i <- readArray arr newI
                        writeArray arr newI elem_j
                        writeArray arr j elem_i
                        partitionLoop newI (j + 1)
                    else partitionLoop i (j + 1)
    partitionLoop (low - 1) low

-- Utility function to verify if list is sorted
isSorted :: (Ord a) => [a] -> Bool
isSorted []       = True
isSorted [_]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)