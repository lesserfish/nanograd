module NN where
import Control.Monad
import System.Random
import Expression
import Data.Matrix


runif :: Int -> Float -> Float -> IO [Expression]
runif n l h = do
  randomValues <- replicateM n (randomRIO (l, h) :: IO Float)
  randomExpressions <- sequence (fmap param randomValues)
  return randomExpressions


data Layer = Layer {nodes :: Matrix Expression, weights :: Matrix Expression, bias :: Expression} deriving Show
data Network = ILayer {layer :: Layer} | HLayer {layer :: Layer, ltail :: Network, activation :: (Expression -> Expression)}

toVector :: Layer -> Matrix Expression
toVector l = (nodes l) <|> (fromList 1 1 [bias l])

initializeNetwork :: Int -> IO Network
initializeNetwork n = do
    node_list <- runif n (-1) 1
    let nodes = fromList 1 n node_list
    let weights = fromList 1 1 [0]
    let bias = 1.0
    let network = ILayer (Layer nodes weights bias)
    return network

pushLayer :: Int -> (Expression -> Expression) -> Network -> IO Network
pushLayer n activation net = do
    let net_ncount = ncols . nodes . layer $ net
    node_list <- runif n (-1) 1
    weight_list <- runif (n * (net_ncount + 1)) (-1) 1
    let nodes = fromList 1 n node_list
    let weights = fromList (net_ncount + 1) n weight_list
    let bias = 1.0
    let network = HLayer (Layer nodes weights bias) net activation
    return network

forwardPass :: Network -> Matrix Expression -> Network
forwardPass (ILayer l) input = ILayer (Layer input (fromList 1 1 [0]) 1.0)
forwardPass (HLayer l tail actv) input = HLayer (Layer new_nodes mat 1.0) net actv where
    new_nodes = fmap actv (vec * mat);
    mat = weights l;
    vec = toVector . layer $ net;
    net = forwardPass tail input;

mse :: Matrix Expression -> Matrix Expression -> Expression
mse a b = sqrt (sum $ fmap (^2) (a - b))

deviation :: Network -> (Matrix Expression -> Matrix Expression -> Expression) -> (Matrix Expression, Matrix Expression) -> Expression
deviation network error (input, expected_output) = (error output expected_output) where
    output = nodes . layer $ evaluation;
    evaluation = forwardPass network input;

overall_deviation :: Network -> (Matrix Expression -> Matrix Expression -> Expression) -> [(Matrix Expression, Matrix Expression)] -> Expression
overall_deviation network error tests =  l * s where
    s = sum ea;
    l = 1.0 / (fromIntegral (length ea) :: Expression);
    ea = fmap (deviation network error) tests;
