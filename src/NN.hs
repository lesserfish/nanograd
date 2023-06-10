module NN where
import Control.Monad
import System.Random
import Expression
import Data.Matrix

runif :: Int -> (Float, Float) -> IO [Expression]
runif n (l, h) = do
  randomValues <- replicateM n (randomRIO (l, h) :: IO Float)
  randomExpressions <- sequence (fmap param randomValues)
  return randomExpressions


data Layer = Layer {nodes :: Matrix Expression, weights :: Matrix Expression, bias :: Expression}
data Network = ILayer {layer :: Layer} | HLayer {layer :: Layer, ltail :: Network, activation :: (Expression -> Expression)}


instance Show Layer where
    show (Layer _nodes _weights _bias) = show _nodes

instance Show Network where
    show (ILayer l) = "\nLayer:\nInput Nodes: \n" ++ show l
    show (HLayer l n a) = "\nLayer:\nNodes: \n" ++ show l ++ "\n Weights: \n" ++ show (weights  l) ++ show n 

params :: [Float] -> IO (Matrix Expression)
params x = sequence . ((fromList 1 (length x)) . (map param)) $ x

toVector :: Layer -> Matrix Expression
toVector l = (nodes l) <|> (fromList 1 1 [bias l])

initializeNetwork :: Int -> IO Network
initializeNetwork n = do
    node_list <- runif n ((-1), 1)
    let nodes = fromList 1 n node_list
    let weights = fromList 1 1 [0]
    let bias = 1.0
    let network = ILayer (Layer nodes weights bias)
    return network

pushLayer :: Int -> (Expression -> Expression) -> Network -> IO Network
pushLayer n activation net = do
    let net_ncount = ncols . nodes . layer $ net
    node_list <- runif n ((-1), 1)
    weight_list <- runif (n * (net_ncount + 1)) ((-1), 1)
    let nodes = fromList 1 n node_list
    let weights = fromList (net_ncount + 1) n weight_list
    let bias = 1.0
    let network = HLayer (Layer nodes weights bias) net activation
    return network

inputSize :: Network -> Int
inputSize (ILayer l) = ncols . nodes $ l
inputSize (HLayer _ t _) = inputSize t

forwardPass :: Network -> Matrix Expression -> Network
forwardPass (ILayer l) input = ILayer (Layer input (fromList 1 1 [0]) 1.0)
forwardPass (HLayer l tail actv) input = HLayer (Layer new_nodes mat 1.0) net actv where
    new_nodes = fmap actv (vec * mat);
    mat = weights l;
    vec = toVector . layer $ net;
    net = forwardPass tail input;

mse :: Matrix Expression -> Matrix Expression -> Expression
mse a b = sqrt ((sum $ fmap (^2) (a - b)) + 0.001)

deviation :: Network -> (Matrix Expression -> Matrix Expression -> Expression) -> (Matrix Expression, Matrix Expression) -> Expression
deviation network error (input, expected_output) = (error output expected_output) where
    output = nodes . layer $ evaluation;
    evaluation = forwardPass network input;

overallDeviation :: Network -> (Matrix Expression -> Matrix Expression -> Expression) -> [(Matrix Expression, Matrix Expression)] -> Expression
overallDeviation network error lio = den * sum (fmap (deviation network error) lio) where
    den = 1.0 / (fromIntegral . length $ lio) :: Expression


updateWeights :: Expression -> (Matrix Expression) -> Float -> IO (Matrix Expression)
updateWeights err mat rate = do
    let bp = backpropagate err :: Expression  -- This is a bug. A hefty bug. A gnarly sickly bug. TODO: FIX. What the fuck. I don't get why this happens.
    let gmat = (fmap (gradient bp) mat) :: Matrix Float
    let fmat = (fmap evaluate mat) :: Matrix Float
    let nmat = fmat - (fmap (*rate) gmat)
    let iomat = (fmap param nmat) :: Matrix (IO Expression)
    emat <- sequence iomat :: IO (Matrix Expression)
--    if isNaN (getElem 1 1 gmat) then putStrLn $ "\nNAMES\n"  ++ show (fmap (name . node) mat) ++ "\nVALUES\n" ++ (show mat) ++ "\nEXPRESSION\n" ++ (fullName bp) ++ "\nEVALUES\n" ++ (fullValue bp) ++ "\nNODES\n" ++ (fullNode bp) else putStr ""
    return emat


updateLayer :: Expression -> Layer -> Float -> IO Layer
updateLayer err l rate = do
    nmat <- updateWeights err (weights l) rate
    let olay = Layer (nodes l) nmat (bias l)
    return olay

updateNetwork :: Expression -> Network -> Float -> IO Network
updateNetwork err (ILayer l) rate = do
    nlay <- updateLayer err l rate
    let onet = ILayer nlay
    return onet

updateNetwork err (HLayer l t act) rate = do
    nlay <- updateLayer err l rate
    ntail <- updateNetwork err t rate
    let onet = HLayer nlay ntail act
    return onet


train :: Int -> Network -> (Matrix Expression, Matrix Expression) -> Float -> IO Network
train 0 net _ _ = return net

train n net (i, o) rate = do
    let err = deviation net mse (i, o)
    putStrLn $ "Iteration " ++ (show n) ++ "   Error: " ++ show (evaluate err)
    nnet <- updateNetwork err net rate
    o <- train (n - 1) nnet (i, o) rate
    return o

mtrain :: Int -> Network -> [(Matrix Expression, Matrix Expression)] -> Float -> IO Network
mtrain 0 net _ _ = return net

mtrain n net arr rate = do
    let err = overallDeviation net mse arr
    putStrLn $ "Iteration " ++ (show n) ++ "   Error: " ++ show (evaluate err)
    nnet <- updateNetwork err net rate
    o <- mtrain (n - 1) nnet arr rate
    return o


generateTrainingSet :: Network -> Int -> (Float, Float) -> IO [(Matrix Expression, Matrix Expression)]
generateTrainingSet network n maxmin
    | n <= 0 = do
        let isize = inputSize network
        ainput <- runif isize maxmin
        let input = fromList 1 isize ainput
        let output = nodes . layer $ (forwardPass network input)
        let io = [(input, output)]
        return io
    | otherwise = do
        let isize = inputSize network
        ainput <- runif isize maxmin
        let input = fromList 1 isize ainput
        let output = nodes . layer $ (forwardPass network input)
        let io = [(input, output)]
        rest <- generateTrainingSet network (n - 1) maxmin
        return $ io ++ rest
