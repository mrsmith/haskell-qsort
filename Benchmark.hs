import QuickSort
import System.CPUTime
import Text.Printf
import Data.List (sort, foldl')
import Control.DeepSeq

main :: IO ()
main = do
    numbers <- map read . words <$> getContents :: IO [Int]
    if null numbers then putStrLn "No input data" else do
        allTimes <- mapM (timeSort numbers) [1..6]
        let times = drop 1 allTimes  -- Drop first run (warmup)
            sorted = quickSort numbers
            sortedTimes = sort times
            medianTime = sortedTimes !! 2  -- median of 5 values (matches C: times[n/2])
            mean = sum times / fromIntegral (length times)
            variance = sum [(t - mean)^2 | t <- times] / 4.0  -- n-1 for sample variance
            stddev = sqrt variance
            cv = (stddev / medianTime) * 100.0  -- coefficient of variation
        putStrLn $ "Sort verified: " ++ if isSorted sorted then "PASSED" else "FAILED"
        putStrLn $ "Elements sorted: " ++ show (length numbers)
        printf "Time taken: %.6f seconds (±%.1f%%)\n" medianTime cv

timeSort :: [Int] -> Int -> IO Double
timeSort numbers multiplier = do
    let modified = map (* multiplier) numbers
        iterations = if length numbers < 1000 then 1000 
                    else if length numbers < 10000 then 100 
                    else 1
    start <- getCPUTime
    let doSort i = let input = map (* (i + multiplier)) modified
                   in quickSort input
        results = map doSort [0..iterations-1]
    deepseq results $ do
        end <- getCPUTime
        return $ fromIntegral (end - start) / (1e12 * fromIntegral iterations)

