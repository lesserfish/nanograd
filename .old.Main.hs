module Main where
import System.Random

generateUniqueID :: IO String
generateUniqueID = do
  randomInt <- randomIO :: IO Int
  return $ show randomInt

{-
class Operation a where
    evaluate :: a -> Float
    grad :: (Operation b) => a -> b -> Float
    uuid :: a -> String

instance Operation Float where
    evaluate x = x
    grad _ _ = 0
    uuid _ = ""

data Value = Value {value :: Float, name :: String} deriving Show
instance Operation Value where
  evaluate = value
  grad x z = if uuid x == uuid z then 1 else 0
  uuid = name

float :: Float -> IO Value
float v = do
    n <- generateUniqueID
    let out = Value v n
    return(out)


data Sum a b = Sum a b deriving Show
instance (Operation a, Operation b) => Operation (Sum a b) where
  evaluate (Sum x y) = evaluate x + evaluate y
  grad (Sum x y) z = (grad x z) + (grad y z)
  uuid _ = ""


(|+|) :: (Operation a, Operation b) => a -> b -> (Sum a b)
(|+|) x y = Sum x y


data Mul a b = Mul a b deriving Show
instance (Operation a, Operation b) => Operation (Mul a b) where
  evaluate (Mul x y) = evaluate x * evaluate y
  grad (Mul x y) z = (grad x z)*(evaluate y) + (evaluate x)*(grad y z)
  uuid _ = ""


(|*|) :: (Operation a, Operation b) => a -> b -> (Mul a b)
(|*|) x y = Mul x y

data Div a b = Div a b deriving Show
instance (Operation a, Operation b) => Operation (Div a b) where
  evaluate (Div x y) = evaluate x / evaluate y
  grad (Div x y) z = ((grad x z)*(evaluate y) - (evaluate x)*(grad y z)) / ((evaluate y)**2)
  uuid _ = ""


(|/|) :: (Operation a, Operation b) => a -> b -> (Div a b)
(|/|) x y = Div x y

-}

data Parameter = Value {value :: Float, name :: String} |
                 Sum {param1 :: Parameter, param2 :: Parameter, name :: String} |
                 Sub {param1 :: Parameter, param2 :: Parameter, name :: String} |
                 Mul {param1 :: Parameter, param2 :: Parameter, name :: String} |
                 Div {param1 :: Parameter, param2 :: Parameter, name :: String} |
                 Pow {param1 :: Parameter, param2 :: Parameter, name :: String} |
                 ReLU {param1 :: Parameter, name :: String} |
                 Exp {param1 :: Parameter, name :: String} deriving Show

param :: Float -> IO Parameter
param f = do
    name <- generateUniqueID
    let output = Value f name
    return(output)

relu :: Parameter -> Parameter
relu p = ReLU p ""

instance Num Parameter where
    pa + pb = Sum pa pb ""
    pa - pb = Sub pa pb ""
    pa * pb = Mul pa pb ""
    fromInteger x = Value (fromInteger x) ""

instance Fractional Parameter where
    pa / pb = Div pa pb ""
    fromRational x = Value (fromRational x) ""

instance Floating Parameter where
    pi = Value pi ""
    exp pa = Exp pa ""
    pa ** pb = Pow pa pb ""

evaluate :: Parameter -> Float
evaluate (Value f _) = f
evaluate (Sum pa pb _) = (evaluate pa) + (evaluate pb)
evaluate (Sub pa pb _) = (evaluate pa) - (evaluate pb)
evaluate (Mul pa pb _) = (evaluate pa) * (evaluate pb)
evaluate (Div pa pb _) = (evaluate pa) / (evaluate pb)
evaluate (Pow pa pb _) = (evaluate pa) ** (evaluate pb)
evaluate (ReLU pa _) = max 0 (evaluate pa)
evaluate (Exp pa _) = exp (evaluate pa)

ngrad :: Parameter -> String -> Float
ngrad (Value f pname) name = if pname == name then 1 else 0
ngrad (Sum fx gx _) name = df + dg where df = (ngrad fx name); dg = (ngrad gx name)
ngrad (Sub fx gx _) name = df - dg where df = (ngrad fx name); dg = (ngrad gx name)
ngrad (Mul fx gx _) name = df*g + f*dg where f = evaluate fx; g = evaluate gx; df = (ngrad fx name); dg = (ngrad gx name)
ngrad (Div fx gx _) name = (df*g - f*dg)/(g**2) where f = evaluate fx; g = evaluate gx; df = (ngrad fx name); dg = (ngrad gx name)
ngrad (Pow fx gx _) name = (f**(g -1))*(g*df + f*(log f)*dg) where f = evaluate fx; g = evaluate gx; df = (ngrad fx name); dg = (ngrad gx name)
ngrad (ReLU fx _) name = if condition then df else 0 where condition = (evaluate fx) >= 0; df = (ngrad fx name)
ngrad (Exp fx _) name = df * exp f where f = evaluate fx; df = ngrad fx name

grad :: Parameter -> Parameter -> Float
grad p1 p2 = ngrad p1 (name p2)

main :: IO ()
main = do
    a <- param (-4.0)
    b <- param (2.0)
    let c = a + b
    let d = a * b + b**3
    let c2 = c + c + 1
    let c3 = c2 + 1 + c2 + (-a)
    let d2 = d + d * 2 + relu(b + a)
    let d3 = d2 + 3 * d2 + relu(b - a)
    let e = c3 - d3
    let f = e*e
    let g = f / 2.0
    let h = g + 10.0 / f
    
    let dhda = grad h a
    let dhdb = grad h b
    putStrLn $ "dh/da = " ++ show dhda
    putStrLn $ "dh/db = " ++ show dhdb
    return ()
