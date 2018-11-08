module Main where

import Test
import TrainingData
import qualified Data.ByteString.Lazy as BL
import Network

main :: IO ()
main = do
    labels <- getLabelsFromFile "trainingdata/train-labels-idx1-ubyte"
    net <- emptyNetwork [784, 15, 10]
    cont <- BL.readFile "trainingdata/train-images-idx3-ubyte" 
    trainedNetwork <- sgd cont (map (fromIntegral) labels) 30 10 3 net
    successRate <- evaluate trainedNetwork
    putStrLn $ show successRate

