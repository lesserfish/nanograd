# THIS IS OUTDATED

For the framework that is currently in development, please check: https://github.com/lesserfish/nanoflow/tree/main


# nanograd

A simple clone of micrograd, but for Haskell.

It is awfully slow, but works.

You can calculate gradients of simple algebraic expressions with ease.
For example:

    a <- param 3.0
    b <- param 5.0
    x = a**2 + 3*a*b + b**3 + sin(a * b)
    bp = backpropagate x
    dxda = gradient bp a -- This should give 17.201561
    dxdb = gradient bp b -- This should give 81.72094
    
    
You can also train small neural networks. For example:

    setStdGen (mkStdGen 42)
    network <- initializeNetwork 1 >>= pushLayer 3 tanh >>= pushLayer 1 tanh
    training_set <- generateTrainingSet network 15 ((-10.0), 10.0)
    let rate = 0.01
    let iterations = 400
    
    model <- initializeNetwork 1 >>= pushLayer 16 tanh >>= pushLayer 1 tanh
    
    result <- mtrain iterations model training_set rate

    putStrLn $ "Original Weights: \n" ++ (show . weights . layer $ network) 
    putStrLn $ "Estimated Weights: \n" ++ (show . weights . layer $ result) 
