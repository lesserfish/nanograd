module Main where
import Data.Matrix
import Expression

main :: IO ()
main = do
    v <- sequence (fromList 1 3 (map param [1, 2, 3]))
    m <- sequence (fromList 3 3 (map param [1..9]))
    let prod = v * m * (transpose v)
    let result = getElem 1 1 prod
    
    putStrLn $ "The result is: " ++ (show (evaluate result))
    return()
