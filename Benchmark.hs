import QuickSort
import System.CPUTime
import System.IO
import Text.Printf

main :: IO ()
main = do
    content <- getContents
    let numbers = map read (words content) :: [Int]
    
    if null numbers
        then putStrLn "No input data"
        else do
            start <- getCPUTime
            let sorted = quickSort numbers
            sorted `seq` do
                end <- getCPUTime
                let diff = fromIntegral (end - start) / (10^12) :: Double
                
                if isSorted sorted
                    then putStrLn "Sort verified: PASSED"
                    else putStrLn "Sort verified: FAILED"
                
                putStrLn $ "Elements sorted: " ++ show (length numbers)
                printf "Time taken: %.6f seconds\n" diff