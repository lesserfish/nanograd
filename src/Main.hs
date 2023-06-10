module Main where
import NN
import Expression
import Data.Matrix
import System.Random

main :: IO()
main = do
    setStdGen (mkStdGen 42)
    network <- initializeNetwork 1 >>= pushLayer 3 tanh >>= pushLayer 1 tanh
    training_set <- generateTrainingSet network 15 ((-10.0), 10.0)
    let rate = 0.01
    let iterations = 400
    
    model <- initializeNetwork 1 >>= pushLayer 16 tanh >>= pushLayer 1 tanh
    
    result <- mtrain iterations model training_set rate

    putStrLn $ "Original Weights: \n" ++ (show . weights . layer $ network) 
    putStrLn $ "Estimated Weights: \n" ++ (show . weights . layer $ result) 
    return ()
--
--
-- TODO: Fix bug.
--main :: IO (Network, [(Matrix Expression, Matrix Expression)], Network)
