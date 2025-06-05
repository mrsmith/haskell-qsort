module QuickSort (quickSort, isSorted) where

-- Using filter
quickSort :: (Ord a) => [a] -> [a]
quickSort []     = []
quickSort (x:xs) = quickSort (filter (<x) xs)
                   ++ [x] ++
                   quickSort (filter (>=x) xs)

-- Utility function to verify if list is sorted
isSorted :: (Ord a) => [a] -> Bool
isSorted []       = True
isSorted [_]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)