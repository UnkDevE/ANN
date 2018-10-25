module Main where

import Image
import Network

main :: IO ()
main = do
    trainingData <- getImagesFromFile "trainingdata/train-images-idx3-ubyte"
    -- let network = sgd (tail trainingData) 30 10 3 $ emptyNetwork [712, 15, 10]
    -- let out = predict network $ head trainingData
    -- putStrLn $ maybe "no result" (show) out

