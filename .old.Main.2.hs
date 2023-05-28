module Main where
import System.Random

generateUniqueID :: IO String
generateUniqueID = do
  randomInt <- randomIO :: IO Int
  return $ show randomInt


class Operation a where
    evaluate :: a -> Float
    grad :: (Operation b) => a -> b -> Float
    uuid :: a -> String

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


main :: IO ()
main = do
    a <- float 3
    b <- float 5
    let x = (a |*| a) |+| (a |*| b)
    let dxda = grad x a
    let dxdb = grad x b
    
    putStrLn $ "dx/da = " ++ show dxda
    putStrLn $ "dx/db = " ++ show dxdb
    return ()
