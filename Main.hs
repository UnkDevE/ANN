module Main where

import Data
import Network

main :: IO ()
main = do
    trainingData <- getTrainingData "trainingdata/train-images-idx3-ubyte" "trainingdata/train-labels-idx3-ubyte"
    net <- emptyNetwork [712, 15, 10]
    trainedNetwork <- sgd (tail trainingData) 30 10 3 net
    let out = predict trainedNetwork $ fst $ head trainingData
    putStrLn $ "actual: " ++ show (snd $ head trainingData) ++ "predicted: " ++ show out

