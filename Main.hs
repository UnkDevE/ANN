module Main where

import Test
import TrainingData
import Network

main :: IO ()
main = do
    labels <- getLabelsFromFile "trainingdata/train-labels-idx3-ubyte"
    net <- emptyNetwork [712, 15, 10]
    trainedNetwork <- sgd "trainingdata/train-images-idx3-ubyte" (map (fromIntegral) labels) 30 10 3 net    
    successRate <- evaluate trainedNetwork
    putStrLn $ show successRate

