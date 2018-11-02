module Test 
(
    evaluate
)
where

import Control.Monad (foldM)
import TrainingData
import Network

evaluate :: Network -> IO Double
evaluate net = do 
    labels <- getLabelsFromFile "trainingdata/t10k-labels-idx1-ubyte"
    highest <- foldM (\acc (l, n) -> do
        image <- loadImage "trainingdata/t10k-images-idx3-ubyte" n 
        if l == predictHighest net image then return $ acc+1 else return acc) 0 $ zip (map (fromIntegral) labels) [0..]
    return $ (fromIntegral highest) / (fromIntegral $ length labels)

