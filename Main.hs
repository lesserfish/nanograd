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
                    Sub {node :: Node, lhs :: Expression, rhs :: Expression} |
                    Mul {node :: Node, lhs :: Expression, rhs :: Expression} |
                    Div {node :: Node, lhs :: Expression, rhs :: Expression} |
                    Pow {node :: Node, lhs :: Expression, rhs :: Expression} |
                    ReLU {node :: Node, lhs :: Expression} |
                    Exp {node :: Node, lhs :: Expression} deriving Show

param :: Float -> IO Expression
param _value = do
    _name <- generateUniqueID
    let _node = Node _name _value 1
    let _expr = Value _node
    return $ _expr

xbackpropagate :: Expression -> Float -> Expression
xbackpropagate (Value _node) _grad = Value (Node (name _node) (value _node) _grad)
xbackpropagate (Sum _node _lhs _rhs) _grad = Sum (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad) (xbackpropagate _rhs _rgrad)
    where _rgrad = _grad; _lgrad = _grad;
xbackpropagate (Sub _node _lhs _rhs) _grad = Sub (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad) (xbackpropagate _rhs _rgrad)
    where _lgrad = _grad; _rgrad = -_grad;
xbackpropagate (Mul _node _lhs _rhs) _grad = Mul (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad) (xbackpropagate _rhs _rgrad)
    where _lgrad = _grad * (value . node $ _rhs); _rgrad = _grad * (value . node $ _lhs);
xbackpropagate (Div _node _lhs _rhs) _grad = Div (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad) (xbackpropagate _rhs _rgrad)
    where _lgrad = _grad/(value . node $ _rhs); _rgrad = _grad * (-1.0 * (value . node $ _lhs))/((value . node $ _rhs)**2)
xbackpropagate (Pow _node _lhs _rhs) _grad = Div (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad) (xbackpropagate _rhs _rgrad)
    where _lgrad = _grad * (b * (a ** (b - 1))); _rgrad = _grad * ((a**b)*(log a)); a = (value . node $ _lhs); b = (value . node $ _rhs)
xbackpropagate (ReLU _node _lhs) _grad = ReLU (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = _grad * (if (value . node $ _lhs) >= 0 then 1 else 0)
xbackpropagate (Exp _node _lhs) _grad = Exp (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = _grad * (exp (value . node $ _lhs))

backpropagate :: Expression -> Expression
backpropagate expr = xbackpropagate expr 1


xsum :: Expression -> Expression -> Expression
xsum expr1 expr2 = Sum (Node _name _value 1) expr1 expr2
    where _value = (value . node $ expr1) + (value . node $ expr2); _name = "(" ++ (name . node $ expr1) ++ " + " ++ (name . node $ expr2) ++ ")"

xsub :: Expression -> Expression -> Expression
xsub expr1 expr2 = Sub (Node _name _value 1) expr1 expr2
    where _value = (value . node $ expr1) - (value . node $ expr2); _name = "(" ++ (name . node $ expr1) ++ " - " ++ (name . node $ expr2) ++ ")"

xmul :: Expression -> Expression -> Expression
xmul expr1 expr2 = Mul (Node _name _value 1) expr1 expr2
    where _value = (value . node $ expr1) * (value . node $ expr2); _name = "(" ++ (name . node $ expr1) ++ " * "++ (name . node $ expr2) ++ ")"

xdiv :: Expression -> Expression -> Expression
xdiv expr1 expr2 = Div (Node _name _value 1) expr1 expr2
    where _value = (value . node $ expr1) / (value . node $ expr2); _name = "(" ++ (name . node $ expr1) ++ " / "++ (name . node $ expr2) ++ ")"

xpow :: Expression -> Expression -> Expression
xpow expr1 expr2 = Pow (Node _name _value 1) expr1 expr2
    where _value = (value . node $ expr1) ** (value . node $ expr2); _name = "(" ++ (name . node $ expr1) ++ " ** "++ (name . node $ expr2) ++ ")"

xrelu :: Expression -> Expression
xrelu expr1 = ReLU (Node _name _value 1) expr1
    where _value = max 0 (value . node $ expr1); _name = "relu(" ++ (name . node $ expr1) ++ ")"

xexp :: Expression -> Expression
xexp expr1 = Exp (Node _name _value 1) expr1
    where _value = exp (value . node $ expr1); _name = "exp(" ++ (name . node $ expr1) ++ ")"

instance Num Expression where
    a + b = xsum a b
    a - b = xsub a b
    a * b = xmul a b
    fromInteger x = Value (Node "" (fromInteger x) 1)

instance Fractional Expression where
    a / b = xdiv a b
    fromRational x = Value (Node "" (fromRational x) 1)

instance Floating Expression where
    pi = Value (Node "" pi 1)
    exp a = xexp a
    a ** b = xpow a b

relu :: Expression -> Expression
relu = xrelu

evaluate :: Expression -> Float
evaluate = value . node

ngrad :: Expression -> String -> Float
ngrad (Value _node) _name = if name _node == _name then grad _node else 0
ngrad (Sum _node _lhs _rhs) _name = if name _node == _name then grad _node else ((ngrad _lhs _name) + (ngrad _rhs _name))
ngrad (Sub _node _lhs _rhs) _name = if name _node == _name then grad _node else ((ngrad _lhs _name) + (ngrad _rhs _name))
ngrad (Mul _node _lhs _rhs) _name = if name _node == _name then grad _node else ((ngrad _lhs _name) + (ngrad _rhs _name))
ngrad (Div _node _lhs _rhs) _name = if name _node == _name then grad _node else ((ngrad _lhs _name) + (ngrad _rhs _name))
ngrad (Pow _node _lhs _rhs) _name = if name _node == _name then grad _node else ((ngrad _lhs _name) + (ngrad _rhs _name))
ngrad (ReLU _node _lhs) _name = if name _node == _name then grad _node else (ngrad _lhs _name)
ngrad (Exp _node _lhs) _name = if name _node == _name then grad _node else (ngrad _lhs _name)

gradient :: Expression -> Expression -> Float
gradient expr1 expr2 = ngrad expr1 (name . node $ expr2)


main :: IO ()
main = putStrLn "Main"
