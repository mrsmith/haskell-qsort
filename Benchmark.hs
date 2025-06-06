import QuickSort
import System.CPUTime
import Text.Printf
import Data.List (sort)

main :: IO ()
main = do
    numbers <- map read . words <$> getContents :: IO [Int]
    if null numbers then putStrLn "No input data" else do
        times <- mapM (timeSort numbers) [1..5]
        let sorted = quickSort numbers
            sortedTimes = sort times
            medianTime = sortedTimes !! 2
            variance = ((last sortedTimes - head sortedTimes) / medianTime) * 100.0
        putStrLn $ "Sort verified: " ++ if isSorted sorted then "PASSED" else "FAILED"
        putStrLn $ "Elements sorted: " ++ show (length numbers)
        printf "Time taken: %.6f seconds (±%.1f%%)\n" medianTime variance

timeSort :: [Int] -> Int -> IO Double
timeSort numbers multiplier = do
    let modified = map (* multiplier) numbers
    start <- getCPUTime
    let sorted = quickSort modified
    length sorted `seq` do
        end <- getCPUTime
        return $ fromIntegral (end - start) / 1e12

median :: [Double] -> Double
median xs = sort xs !! 2