module Main where
import NN
import Expression
import Data.Matrix

main :: IO()
main = do
    network <- initializeNetwork 1 >>= pushLayer 1 id
    training_set <- generateTrainingSet network 1 ((-10.0), 10.0)
    let rate = 0.001
    let iterations = 5000
    
    model <- initializeNetwork 1 >>= pushLayer 1 id
    
    result <- debugtrain iterations model training_set rate

    putStrLn $ "Original Weights: \n" ++ (show . weights . layer $ network) 
    putStrLn $ "Estimated Weights: \n" ++ (show . weights . layer $ result) 
    return ()
--
--
-- TODO: Fix bug.
--main :: IO (Network, [(Matrix Expression, Matrix Expression)], Network)
