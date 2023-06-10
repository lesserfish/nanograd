module Expression where
import System.Random
import Data.Hashable


-- Generates a name so you don't have to manually name your variables.
generateUniqueID :: IO String
generateUniqueID = do
  randomInt <- randomIO :: IO Int
  return $ show randomInt

hashString :: String -> String
hashString = show . hash


data Node = Node {name :: String, value :: Float, grad :: Float} deriving Show
data Expression =   Value {node :: Node} |
                    Sum {node :: Node, lhs :: Expression, rhs :: Expression} |
                    Sub {node :: Node, lhs :: Expression, rhs :: Expression} |
                    Mul {node :: Node, lhs :: Expression, rhs :: Expression} |
                    Div {node :: Node, lhs :: Expression, rhs :: Expression} |
                    Pow {node :: Node, lhs :: Expression, rhs :: Expression} |
                    ReLU {node :: Node, lhs :: Expression} |
                    Exp {node :: Node, lhs :: Expression} |
                    Log {node :: Node, lhs :: Expression} |
                    Sin {node :: Node, lhs :: Expression} |
                    Cos {node :: Node, lhs :: Expression} |
                    Asin {node :: Node, lhs :: Expression} |
                    Acos {node :: Node, lhs :: Expression} |
                    Atan {node :: Node, lhs :: Expression} |
                    Sinh {node :: Node, lhs :: Expression} |
                    Cosh {node :: Node, lhs :: Expression} |
                    Asinh {node :: Node, lhs :: Expression} |
                    Acosh {node :: Node, lhs :: Expression} |
                    Atanh {node :: Node, lhs :: Expression} |
                    Abs {node :: Node, lhs :: Expression} |
                    Signum {node :: Node, lhs :: Expression} 


{--
fullName :: Expression -> String
fullName (Value _node) = if (name _node == "") then (show . value $ _node) else "(" ++ (show . name $ _node) ++ ")"
fullName (Sum _node _lhs _rhs) = "(" ++ (fullName _lhs) ++ " + " ++ (fullName _rhs) ++ ")"
fullName (Sub _node _lhs _rhs) = "(" ++ (fullName _lhs) ++ " - " ++ (fullName _rhs) ++ ")"
fullName (Mul _node _lhs _rhs) = "(" ++ (fullName _lhs) ++ " * " ++ (fullName _rhs) ++ ")"
fullName (Div _node _lhs _rhs) = "(" ++ (fullName _lhs) ++ " / " ++ (fullName _rhs) ++ ")"
fullName (Pow _node _lhs _rhs) = "(" ++ (fullName _lhs) ++ " ** " ++ (fullName _rhs) ++ ")"
fullName (ReLU _node _lhs)  = "relu(" ++ (fullName _lhs) ++ ")"
fullName (Exp _node _lhs) = "exp(" ++ (fullName _lhs) ++ ")"
fullName (Log _node _lhs) = "log(" ++ (fullName _lhs) ++ ")"
fullName (Sin _node _lhs) = "sin(" ++ (fullName _lhs) ++ ")"
fullName (Cos _node _lhs) = "cos(" ++ (fullName _lhs) ++ ")"
fullName (Asin _node _lhs) = "asin(" ++ (fullName _lhs) ++ ")"
fullName (Acos _node _lhs) = "acos(" ++ (fullName _lhs) ++ ")"
fullName (Sinh _node _lhs) = "sinh(" ++ (fullName _lhs) ++ ")"
fullName (Cosh _node _lhs) = "cosh(" ++ (fullName _lhs) ++ ")"
fullName (Asinh _node _lhs) = "asinh(" ++ (fullName _lhs) ++ ")"
fullName (Acosh _node _lhs) = "acosh(" ++ (fullName _lhs) ++ ")"
fullName (Atanh _node _lhs) = "atanh(" ++ (fullName _lhs) ++ ")"
fullName (Abs _node _lhs) = "abs(" ++ (fullName _lhs) ++ ")"
fullName (Signum _node _lhs) = "signum(" ++ (fullName _lhs) ++ ")"


fullValue :: Expression -> String
fullValue (Value _node) = "(" ++ (show . value $ _node) ++ ")"
fullValue (Sum _node _lhs _rhs) = "(" ++ (fullValue _lhs) ++ " + " ++ (fullValue _rhs) ++ ")"
fullValue (Sub _node _lhs _rhs) = "(" ++ (fullValue _lhs) ++ " - " ++ (fullValue _rhs) ++ ")"
fullValue (Mul _node _lhs _rhs) = "(" ++ (fullValue _lhs) ++ " * " ++ (fullValue _rhs) ++ ")"
fullValue (Div _node _lhs _rhs) = "(" ++ (fullValue _lhs) ++ " / " ++ (fullValue _rhs) ++ ")"
fullValue (Pow _node _lhs _rhs) = "(" ++ (fullValue _lhs) ++ " ** " ++ (fullValue _rhs) ++ ")"
fullValue (ReLU _node _lhs)  = "relu(" ++ (fullValue _lhs) ++ ")"
fullValue (Exp _node _lhs) = "exp(" ++ (fullValue _lhs) ++ ")"
fullValue (Log _node _lhs) = "log(" ++ (fullValue _lhs) ++ ")"
fullValue (Sin _node _lhs) = "sin(" ++ (fullValue _lhs) ++ ")"
fullValue (Cos _node _lhs) = "cos(" ++ (fullValue _lhs) ++ ")"
fullValue (Asin _node _lhs) = "asin(" ++ (fullValue _lhs) ++ ")"
fullValue (Acos _node _lhs) = "acos(" ++ (fullValue _lhs) ++ ")"
fullValue (Sinh _node _lhs) = "sinh(" ++ (fullValue _lhs) ++ ")"
fullValue (Cosh _node _lhs) = "cosh(" ++ (fullValue _lhs) ++ ")"
fullValue (Asinh _node _lhs) = "asinh(" ++ (fullValue _lhs) ++ ")"
fullValue (Acosh _node _lhs) = "acosh(" ++ (fullValue _lhs) ++ ")"
fullValue (Atanh _node _lhs) = "atanh(" ++ (fullValue _lhs) ++ ")"
fullValue (Abs _node _lhs) = "abs(" ++ (fullValue _lhs) ++ ")"
fullValue (Signum _node _lhs) = "signum(" ++ (fullValue _lhs) ++ ")"

fullNode :: Expression -> String
fullNode (Value _node) = "\n" ++ show _node ++ "\n"
fullNode (Sum _node _lhs _rhs) = (fullNode _lhs) ++ (fullNode _rhs)
fullNode (Sub _node _lhs _rhs) = (fullNode _lhs) ++ (fullNode _rhs)
fullNode (Mul _node _lhs _rhs) = (fullNode _lhs) ++ (fullNode _rhs)
fullNode (Div _node _lhs _rhs) = (fullNode _lhs) ++ (fullNode _rhs)
fullNode (Pow _node _lhs _rhs) = (fullNode _lhs) ++ (fullNode _rhs)
fullNode (ReLU _node _lhs)  = (fullNode _lhs)
fullNode (Exp _node _lhs) = (fullNode _lhs)
fullNode (Log _node _lhs) = (fullNode _lhs)
fullNode (Sin _node _lhs) = (fullNode _lhs)
fullNode (Cos _node _lhs) = (fullNode _lhs)
fullNode (Asin _node _lhs) = (fullNode _lhs)
fullNode (Acos _node _lhs) = (fullNode _lhs)
fullNode (Sinh _node _lhs) = (fullNode _lhs)
fullNode (Cosh _node _lhs) = (fullNode _lhs)
fullNode (Asinh _node _lhs) = (fullNode _lhs)
fullNode (Acosh _node _lhs) = (fullNode _lhs)
fullNode (Atanh _node _lhs) = (fullNode _lhs)
fullNode (Abs _node _lhs) = (fullNode _lhs)
fullNode (Signum _node _lhs) = (fullNode _lhs) 
--}


param :: Float -> IO Expression
param _value = do
    _name <- generateUniqueID
    let _node = Node _name _value 1
    let _expr = Value _node
    return $ _expr

collapse :: Expression -> Expression
collapse expr = Value (Node (name . node $ expr) (value . node $ expr) 1)

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

xbackpropagate (Pow _node _lhs _rhs) _grad = Pow (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad) (xbackpropagate _rhs _rgrad)
    where _lgrad = _grad * (b * (a ** (b - 1))); _rgrad = _grad * ((a**b)*(log a)); a = (value . node $ _lhs); b = (value . node $ _rhs)

xbackpropagate (ReLU _node _lhs) _grad = ReLU (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = _grad * (if (value . node $ _lhs) >= 0 then 1 else 0)

xbackpropagate (Exp _node _lhs) _grad = Exp (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = _grad * (exp (value . node $ _lhs))

xbackpropagate (Log _node _lhs) _grad = Log (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = _grad / (value . node $ _lhs)

xbackpropagate (Sin _node _lhs) _grad = Sin (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = _grad * cos (value . node $ _lhs)

xbackpropagate (Cos _node _lhs) _grad = Cos (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = -_grad * sin(value . node $ _lhs)

xbackpropagate (Asin _node _lhs) _grad = Asin (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = _grad / sqrt(1 - z**2); z = value . node $ _lhs;

xbackpropagate (Acos _node _lhs) _grad = Acos (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = -_grad / sqrt(1 - z**2); z = value . node $ _lhs;

xbackpropagate (Atan _node _lhs) _grad = Atan (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = _grad / (1 + z**2); z = value . node $ _lhs;

xbackpropagate (Sinh _node _lhs) _grad = Sinh (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = _grad * cosh (value . node $ _lhs)

xbackpropagate (Cosh _node _lhs) _grad = Cosh (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = _grad * sinh (value . node $ _lhs)

xbackpropagate (Asinh _node _lhs) _grad = Asinh (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = _grad / sqrt(z**2 + 1); z = value . node $ _lhs;

xbackpropagate (Acosh _node _lhs) _grad = Acosh (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = _grad / sqrt(z**2 - 1); z = value . node $ _lhs;

xbackpropagate (Atanh _node _lhs) _grad = Atanh (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = _grad / (1 - z**2); z = value . node $ _lhs;

xbackpropagate (Abs _node _lhs) _grad = Abs (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = _grad * (if (value . node $ _lhs) >= 0 then 1 else -1)

xbackpropagate (Signum _node _lhs) _grad = Signum (Node (name _node) (value _node) _grad) (xbackpropagate _lhs _lgrad)
    where _lgrad = 0

backpropagate :: Expression -> Expression
backpropagate expr = xbackpropagate expr 1

xsum :: Expression -> Expression -> Expression
xsum expr1 expr2 = Sum (Node _name _value 1) expr1 expr2
    where _value = (value . node $ expr1) + (value . node $ expr2); _name = hashString ("(" ++ (name . node $ expr1) ++ " + " ++ (name . node $ expr2) ++ ")")

xsub :: Expression -> Expression -> Expression
xsub expr1 expr2 = Sub (Node _name _value 1) expr1 expr2
    where _value = (value . node $ expr1) - (value . node $ expr2); _name = hashString ("(" ++ (name . node $ expr1) ++ " - " ++ (name . node $ expr2) ++ ")")

xmul :: Expression -> Expression -> Expression
xmul expr1 expr2 = Mul (Node _name _value 1) expr1 expr2
    where _value = (value . node $ expr1) * (value . node $ expr2); _name = hashString ("(" ++ (name . node $ expr1) ++ " * "++ (name . node $ expr2) ++ ")")

xdiv :: Expression -> Expression -> Expression
xdiv expr1 expr2 = Div (Node _name _value 1) expr1 expr2
    where _value = (value . node $ expr1) / (value . node $ expr2); _name = hashString ("(" ++ (name . node $ expr1) ++ " / "++ (name . node $ expr2) ++ ")")

xpow :: Expression -> Expression -> Expression
xpow expr1 expr2 = Pow (Node _name _value 1) expr1 expr2
    where _value = (value . node $ expr1) ** (value . node $ expr2); _name = hashString ("(" ++ (name . node $ expr1) ++ " ** "++ (name . node $ expr2) ++ ")")

xrelu :: Expression -> Expression
xrelu expr1 = ReLU (Node _name _value 1) expr1
    where _value = max 0 (value . node $ expr1); _name = hashString ("relu(" ++ (name . node $ expr1) ++ ")")

xexp :: Expression -> Expression
xexp expr1 = Exp (Node _name _value 1) expr1
    where _value = exp (value . node $ expr1); _name = hashString ("exp(" ++ (name . node $ expr1) ++ ")")

xlog :: Expression -> Expression
xlog expr1 = Log (Node _name _value 1) expr1
    where _value = log (value . node $ expr1); _name = hashString ("log(" ++ (name . node $ expr1) ++ ")")

xsin :: Expression -> Expression
xsin expr1 = Sin (Node _name _value 1) expr1
    where _value = sin (value . node $ expr1); _name = hashString ("sin(" ++ (name . node $ expr1) ++ ")")

xcos :: Expression -> Expression
xcos expr1 = Cos (Node _name _value 1) expr1
    where _value = cos (value . node $ expr1); _name = hashString ("cos(" ++ (name . node $ expr1) ++ ")")

xasin :: Expression -> Expression
xasin expr1 = Asin (Node _name _value 1) expr1
    where _value = asin (value . node $ expr1); _name = hashString ("asin(" ++ (name . node $ expr1) ++ ")")

xacos :: Expression -> Expression
xacos expr1 = Acos (Node _name _value 1) expr1
    where _value = acos (value . node $ expr1); _name = hashString ("acos(" ++ (name . node $ expr1) ++ ")")

xatan :: Expression -> Expression
xatan expr1 = Atan (Node _name _value 1) expr1
    where _value = atan (value . node $ expr1); _name = hashString ("atan(" ++ (name . node $ expr1) ++ ")")

xsinh :: Expression -> Expression
xsinh expr1 = Sinh (Node _name _value 1) expr1
    where _value = sinh (value . node $ expr1); _name = hashString ("sinh(" ++ (name . node $ expr1) ++ ")")

xcosh :: Expression -> Expression
xcosh expr1 = Cosh (Node _name _value 1) expr1
    where _value = cosh (value . node $ expr1); _name = hashString ("cosh(" ++ (name . node $ expr1) ++ ")")

xasinh :: Expression -> Expression
xasinh expr1 = Asinh (Node _name _value 1) expr1
    where _value = asinh (value . node $ expr1); _name = hashString ("asinh(" ++ (name . node $ expr1) ++ ")")

xacosh :: Expression -> Expression
xacosh expr1 = Acosh (Node _name _value 1) expr1
    where _value = acosh (value . node $ expr1); _name = hashString ("acosh(" ++ (name . node $ expr1) ++ ")")

xatanh :: Expression -> Expression
xatanh expr1 = Atanh (Node _name _value 1) expr1
    where _value = atanh (value . node $ expr1); _name = hashString ("atanh(" ++ (name . node $ expr1) ++ ")")

xabs :: Expression -> Expression
xabs expr1 = Abs (Node _name _value 1) expr1
    where _value = abs (value . node $ expr1); _name = hashString ("abs(" ++ (name . node $ expr1) ++ ")")

xsignum :: Expression -> Expression
xsignum expr1 = Signum (Node _name _value 1) expr1
    where _value = signum (value . node $ expr1); _name = hashString ("signum(" ++ (name . node $ expr1) ++ ")")

instance Num Expression where
    (+) = xsum
    (-) = xsub
    (*) = xmul
    fromInteger x = Value (Node "" (fromInteger x) 1)
    abs = xabs
    signum = xsignum

instance Fractional Expression where
    (/) = xdiv
    fromRational x = Value (Node "" (fromRational x) 1)

instance Floating Expression where
    (**) = xpow
    pi = Value (Node "" pi 1)
    exp = xexp
    log = xlog
    cos = xcos
    sin = xsin
    acos = xacos
    asin = xasin
    atan = xatan
    sinh = xsinh
    cosh = xcosh
    asinh = xasinh
    acosh = xacosh
    atanh = xatanh

instance Show Expression where
    show exp = show (evaluate exp)

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
ngrad (Log _node _lhs) _name = if name _node == _name then grad _node else (ngrad _lhs _name)
ngrad (Sin _node _lhs) _name = if name _node == _name then grad _node else (ngrad _lhs _name)
ngrad (Cos _node _lhs) _name = if name _node == _name then grad _node else (ngrad _lhs _name)
ngrad (Asin _node _lhs) _name = if name _node == _name then grad _node else (ngrad _lhs _name)
ngrad (Acos _node _lhs) _name = if name _node == _name then grad _node else (ngrad _lhs _name)
ngrad (Atan _node _lhs) _name = if name _node == _name then grad _node else (ngrad _lhs _name)
ngrad (Sinh _node _lhs) _name = if name _node == _name then grad _node else (ngrad _lhs _name)
ngrad (Cosh _node _lhs) _name = if name _node == _name then grad _node else (ngrad _lhs _name)
ngrad (Asinh _node _lhs) _name = if name _node == _name then grad _node else (ngrad _lhs _name)
ngrad (Acosh _node _lhs) _name = if name _node == _name then grad _node else (ngrad _lhs _name)
ngrad (Atanh _node _lhs) _name = if name _node == _name then grad _node else (ngrad _lhs _name)
ngrad (Abs _node _lhs) _name = if name _node == _name then grad _node else (ngrad _lhs _name)
ngrad (Signum _node _lhs) _name = if name _node == _name then grad _node else (ngrad _lhs _name)

gradient :: Expression -> Expression -> Float
gradient expr1 expr2 = _value where
    _value = (ngrad expr1 (name . node $ expr2));

