import QuickSort
import System.CPUTime
import Text.Printf

main :: IO ()
main = do
    numbers <- map read . words <$> getContents :: IO [Int]
    if null numbers then putStrLn "No input data" else do
        start <- getCPUTime
        let sorted = quickSort numbers
        sorted `seq` do
            end <- getCPUTime
            let diff = fromIntegral (end - start) / 1e12
            putStrLn $ "Sort verified: " ++ if isSorted sorted then "PASSED" else "FAILED"
            putStrLn $ "Elements sorted: " ++ show (length numbers)
            printf "Time taken: %.6f seconds\n" diff