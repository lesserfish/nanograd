module Main where
import NN
import Expression
import Data.Matrix

main :: IO()
main = do
    network <- initializeNetwork 1 >>= pushLayer 1 id
    training_set <- generateTrainingSet network 10 ((-10.0), 10.0)
    let rate = 0.0001
    let iterations = 10000
    
    model <- initializeNetwork 1 >>= pushLayer 1 id
    
    result <- mtrain iterations model training_set rate

    putStrLn $ "Original Weights: \n" ++ (show . weights . layer $ network) 
    putStrLn $ "Estimated Weights: \n" ++ (show . weights . layer $ result) 
    return ()
--
--
--
-- TODO: Fix bug.
--main :: IO (Network, [(Matrix Expression, Matrix Expression)], Network)
--    return (network, training_set, result)
