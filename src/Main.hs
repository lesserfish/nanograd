module Main where
import Data.Matrix
import Expression
import Text.Printf


z1, z2, z3 :: Fractional a => a
z1 = -3.2
z2 = 12.4
z3 = -0.7

model :: (Fractional a) => (a, a, a) -> (a, a, a) -> a
model (w1, w2, w3) (x, y, z) = w1*x + w2*y + w3*z

func :: (Fractional a) => (a, a, a) -> a
func = model (z1, z2,z3)

test_input :: [(Float, Float, Float)]
test_input = [(1, 2, 3), (-1, 2, 3), (-4, 5, 6), (-3, -5, 2), (3, 2, 1)]
test_output :: [Float]
test_output = fmap func test_input

convertInput :: [(Float, Float, Float)] -> [(Expression, Expression, Expression)]
convertInput = map (\(x, y, z) -> (fromRational (toRational x), fromRational (toRational y), fromRational (toRational z)))

convertPrediction :: [Float] -> [Expression]
convertPrediction = fmap (fromRational . toRational)

optimize :: (Expression, Expression, Expression) -> (Expression, Expression, Expression)
optimize (w1, w2, w3) = (w1', w2', w3') where
    input = convertInput test_input;
    expectation = convertPrediction test_output;
    prediction = fmap (model (w1, w2, w3)) input;
    mse = sum (zipWith (\x y -> (x - y)**2) prediction expectation);
    grads = backpropagate mse;
    dw1 = gradient grads w1;
    dw2 = gradient grads w2;
    dw3 = gradient grads w3;
    w1' = w1 - (fromRational . toRational $ (rate * dw1));
    w2' = w2 - (fromRational . toRational $ (rate * dw2));
    w3' = w3 - (fromRational . toRational $ (rate * dw3));
    rate = 0.001;


main = loop 1000 initialInput
  where
    initialInput :: (Float, Float, Float)
    initialInput = (1.0, 1.0, 1.0)

    loop :: Int -> (Float, Float, Float) -> IO ()
    loop 0 _ = return ()
    loop n (f1, f2,f3) = do
      w1 <- param f1
      w2 <- param f2
      w3 <- param f3
      let (w1', w2', w3') = optimize (w1, w2, w3)
      let dw1 = evaluate w1'
      let dw2 = evaluate w2'
      let dw3 = evaluate w3'
      let output = (dw1, dw2, dw3)
      let error = sqrt ((dw1 - z1)**2 + (dw2 - z2)**2 + (dw3 - z3)**2)
      putStrLn $ printf "%.3f, %.3f, %.3f - error: %.3f" (dw1) (dw2) (dw3) error
      loop (n - 1) output

