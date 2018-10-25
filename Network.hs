module Network 
(
    sgd
    emptyNetwork
    predict
    Network (..)
)
where

import System.Random
import System.Random.Shuffle (shuffle)
import Data.List (transpose)
import Data.List.Split (chunksOf)

predict :: Network -> [Double] -> [Double]
predict (Network xs _) a = scanl (\acc (w, b) -> sigmoid $ (acc * w) + b) a xs

sigmoid z = 1 / (1 + exp (-z))

sgd :: [([Double], Int)] -> Int -> Int -> Double -> Network -> Network
sgd trainingData epochs minibatchSize eta net = do
    gen <- getStdGen
    let shuffled = shuffle trainingData $ randomRs (0, 2) gen
    let newNet = foldr (\net batch -> updateMiniBatch net batch eta) net $ chunksOf minibatchSize shuffled
    sgd trainingData (epochs-1) minibatchSize eta newNet

sgd trainingData 0 minibatchSize eta net = net

updateMiniBatch :: Network -> [([Double], Int)] -> Double -> Network
updateMiniBatch (Network xs sizes) miniBatch eta =
    zipWith (\x y -> mapTuple ((-) eta/length miniBatch) $ mapTuple (* y) x) 
        foldr (\Network acc s) n -> Network (zipWith (mapTuple (+)) acc n) s) 
            $ map (networkError net) miniBatch

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

networkError :: Network -> ([Double], Int) -> [Double]
networkError (Network xs sizes) ex@(x, y) = 
    concat $ snd $ unzip $ scanr (\(ws, es) w -> 
        zip w $ zipWith (*) (map sigmoidPrime x) $ zipWith (*) ws es) 
    (outputError net ex) transpose $ unflatten (fst $ unzip xs) sizes

flatTranspose (Num a) => [a] -> [Int] -> [a]
flatTranspose xs sizes = concat $ transpose $ unflatten xs sizes

unflatten :: (Num a) => [a] -> [Int] -> [[a]]
unflatten xs (size:sizes) = take size xs:unflatten (drop size xs) sizes
unflatten xs [] = [] 

outputError :: Network -> ([Double], Int) -> [Double]
outputError net (x, y) = 
    zipWith (*) (map sigmoidPrime x) $ zipWith (-) (predict net x) $ actual y $ length x

actual :: (Num a) => Int -> Int ->  a 
actual n size = take size [if x == 0 then 1 else 0 | x <- [n, (n-1)..]]

emptyNetwork :: [Int] -> Network
emptyNetwork sizes = do
    gen <- getStdGen
    let x = sum sizes
    Network (zip (take x $ randomRs (0::Double, 1::Double) gen) (take x $ repeat 0)) sizes

data Network = Network [(Double, Double)] [Int]
