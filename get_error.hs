setStdGen (mkStdGen 123456)
(original_network, training_set, failed_network) <- main
err = overallDeviation failed_network mse training_set
bp = backpropagate err
critical = rhs . lhs . rhs $ bp

