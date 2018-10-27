module Network 
(
    sgd,
    emptyNetwork,
    predict,
    Network (..)
)
where

import System.Random
import System.Random.Shuffle (shuffle)
import Data.List (transpose, foldl')
import Data.List.Split (chunksOf)

predict :: Network -> [Double] -> [Double]
predict (Network xs _) a = foldl' (\acc (w, b) -> map (\act -> sigmoid $ (act*w) + b) acc) a xs

sigmoid z = 1 / (1 + exp (-z))

sgd :: [([Double], Int)] -> Int -> Int -> Double -> Network -> IO Network
sgd trainingData 0 minibatchSize eta net = return net
sgd trainingData epochs minibatchSize eta net = do
    gen <- getStdGen
    let shuffled = shuffle trainingData $ randomRs (0, 2) gen
    let newNet = foldl (\net batch -> updateMiniBatch net batch eta) net $ chunksOf minibatchSize shuffled
    sgd trainingData (epochs-1) minibatchSize eta newNet

updateMiniBatch :: Network -> [([Double], Int)] -> Double -> Network
updateMiniBatch net@(Network xs sizes) miniBatch eta =
    Network (zipWith (\x y -> mapTuple ((-) (eta/(fromIntegral $ length miniBatch)) . (* y)) x) xs
        $ foldr (\acc n -> zipWith (+) acc n) (repeat 0) 
            $ map (\batch -> zipWith (*) (predict net (fst batch)) $ networkError net batch) miniBatch) sizes

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

networkError :: Network -> ([Double], Int) -> [Double]
networkError net@(Network xs sizes) ex@(x, y) = 
    foldl' (\errors ws -> zipWith (*) (map (sigmoidPrime) x) $ zipWith (*) errors ws)
        (outputError net ex) $ transpose $ unflatten (fst $ unzip xs) sizes

sigmoidPrime z = sigmoid z * (1-sigmoid z)

unflatten :: [Double] -> [Int] -> [[Double]]
unflatten xs (size:sizes) = take size xs:unflatten (drop size xs) sizes
unflatten xs [] = [] 

outputError :: Network -> ([Double], Int) -> [Double]
outputError net (x, y) = 
    zipWith (*) (map (sigmoidPrime) x) $ zipWith (-) (predict net x) $ map (fromIntegral) $ actual y 

actual :: (Num a) => Int -> [a] 
actual n = [if x == 0 then 1 else 0 | x <- [n, (n-1)..]]

emptyNetwork :: [Int] -> IO Network
emptyNetwork sizes = do
    gen <- getStdGen
    let x = sum sizes
    return $ Network (zip (take x $ randomRs (0::Double, 1::Double) gen) (take x $ repeat 0)) sizes

data Network = Network [(Double, Double)] [Int]
