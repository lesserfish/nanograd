module Main where
import System.Random


-- Generates a name so you don't have to manually name your variables.
generateUniqueID :: IO String
generateUniqueID = do
  randomInt <- randomIO :: IO Int
  return $ show randomInt

data Node = Node {name :: String, value :: Float, grad :: Float} deriving Show
data Expression =   Value {node :: Node} |
                    Sum {node :: Node, lhs :: Expression, rhs :: Expression} |
                    Mul {node :: Node, lhs :: Expression, rhs :: Expression} deriving Show

param :: Float -> IO Expression
param _value = do
    _name <- generateUniqueID
    let _node = Node _name _value 1
    let _expr = Value _node
    return $ _expr

xbackpropagate :: Expression -> Float -> Expression
xbackpropagate (Value _node) _grad = Value (Node (name _node) (value _node) _grad)
xbackpropagate (Sum _node _lhs _rhs) _grad = Sum (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _rgrad) (xbackpropagate _rhs _lgrad)
    where _rgrad = _grad; _lgrad = _grad;
xbackpropagate (Mul _node _lhs _rhs) _grad = Mul (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad) (xbackpropagate _rhs _rgrad)
    where _lgrad = _grad * (value . node $ _rhs); _rgrad = _grad * (value . node $ _lhs);

backpropagate :: Expression -> Expression
backpropagate expr = xbackpropagate expr 1


xsum :: Expression -> Expression -> Expression
xsum expr1 expr2 = Sum (Node _name _value 1) expr1 expr2
    where _value = (value . node $ expr1) + (value . node $ expr2); _name = "(" ++ (name . node $ expr1) ++ " + " ++ (name . node $ expr2) ++ ")"

xmul :: Expression -> Expression -> Expression
xmul expr1 expr2 = Mul (Node _name _value 1) expr1 expr2
    where _value = (value . node $ expr1) * (value . node $ expr2); _name = "(" ++ (name . node $ expr1) ++ " * "++ (name . node $ expr2) ++ ")"

evaluate :: Expression -> Float
evaluate = value . node

ngrad :: Expression -> String -> Float
ngrad (Value _node) _name = if name _node == _name then grad _node else 0
ngrad (Sum _node _lhs _rhs) _name = if name _node == _name then grad _node else ((ngrad _lhs _name) + (ngrad _rhs _name))
ngrad (Mul _node _lhs _rhs) _name = if name _node == _name then grad _node else ((ngrad _lhs _name) + (ngrad _rhs _name))

gradient :: Expression -> Expression -> Float
gradient expr1 expr2 = ngrad expr1 (name . node $ expr2)

main :: IO ()
main = putStrLn "Main"
