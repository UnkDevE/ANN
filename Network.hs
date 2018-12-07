module Network 
(
    sgd,
    networkError,
    updateMiniBatch,
    outputError,
    predict,
    emptyNetwork,
    predictHighest,
    activations,
    Network (..)
)
where

import TrainingData
import Control.Monad (foldM)
import System.Random (getStdGen, randomRs)
import System.Random.Shuffle (shuffle')
import qualified Data.ByteString.Lazy as BL
import Data.List (transpose, foldl')
import Data.List.Split (chunksOf)

import Debug.Trace

predictHighest :: Network -> [Double] -> Int
predictHighest net a = snd $ foldl' (\p@(acc, _) n@(a, _) -> if acc < a then n else p) (0, 0) $ zip (predict net a) [0..]

predict :: Network -> [Double] -> [Double]
predict (Network neurons) a = 
    foldl' (foldl' (\act (Neuron ws b) -> map (sigmoid . (+ b)) (zipWith (*) ws act)) a neurons 

matrixVectorProduct :: (Num a) => [[a]] -> [a] -> [a]
matrixVectorProduct xxs xs = map (dot xs) xxs

dot :: (Num a) => [a] -> [a] -> a
dot xs ys = sum $ zipWith (*) xs ys 

sigmoid z = 1 / (1 + exp (-z))

sgd :: BL.ByteString -> [Int] -> Int -> Int -> Double -> Network -> IO Network
sgd imagesCont labels 0 minibatchSize eta net = return net
sgd imagesCont labels epochs minibatchSize eta net = do
    gen <- getStdGen
    let shuffled = shuffle' (zip labels [0..]) (length labels) gen
    newNet <- foldM (\net batch -> do 
                            let (ls, ns) = unzip batch
                            images <- loadBatch imagesCont ns 
                            return $ updateMiniBatch net (zip images ls) eta) net $ chunksOf minibatchSize shuffled
    sgd imagesCont labels (epochs-1) minibatchSize eta newNet

updateMiniBatch :: Network -> [([Double], Int)] -> Double -> Network
updateMiniBatch net@(Network ws bs) miniBatch eta =
    Network (zipWith (subMatrix) ws $ map ((flip multiplyMatrix) nm)
                $ map (\weight -> (foldr (\(err, act) nxt -> sumMatrix nxt $ multiplyMatrices act err) weight
                        (zip (map (networkError net) miniBatch) $ map (transpose . activations net) $ fst $ unzip $ miniBatch)))
                        ws)
            $ zipWith (zipWith (-)) bs $ repeat $ matrixVectorProduct 
                (foldr (\acc nxt -> sumMatrix acc nxt) [] $ map (networkError net) miniBatch) 
                $ repeat nm
    where nm = (eta / (fromIntegral $ length miniBatch))

subMatrix :: (Num a) => [[a]] -> [[a]] -> [[a]]
subMatrix xxs yys = map (\(x,y) -> zipWith (-) x y) $ zip xxs yys

sumMatrix :: (Num a) => [[a]] -> [[a]] -> [[a]]
sumMatrix xxs yys = map (\(x,y) -> zipWith (+) x y) $ zip xxs yys

multiplyMatrix :: (Num a) => [[a]] -> a -> [[a]]
multiplyMatrix xxs y = map (map (* y)) xxs

multiplyMatrices :: (Num a) => [[a]] -> [[a]] -> [[a]]
multiplyMatrices xxs yys = 
    map (\xs -> map (dot xs) $ transpose yys) xxs

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

networkError :: Network -> Neuron -> [[Double]]
networkError net@(Network neurons) Neuron(w b) = scanr (\act ws 
        
toWeights :: Network -> [[[Double]]]
toWeights (Network neurons) = 

sigmoidPrime z = z * (1 - z)

activations :: Network -> [Double] -> [[Double]]
activations (Network neurons) a = 
    scanl (foldl' (\act (Neuron ws b) -> map (sigmoid . (+ b)) (zipWith (*) ws act)) 
    
outputError :: Network -> Neuron -> [Double]
outputError net Neuron(w b) =
    zipWith (*) (zipWith (-) (predict net w) $ actual b) $ map (sigmoidPrime) $ predict net w
    
actual :: (Num a) => Int -> [a] 
actual n = [if x == 0 then 1 else 0 | x <- [n, (n-1)..]]

emptyNetwork :: [Int] -> IO Network
emptyNetwork sizes = 
    map (map (emptyNeuron) sizes) sizes

emptyNeuron :: Int -> IO Neuron 
emptyNeuron size = do
    gen <- getStdGen 
    let rands = randomRs (0::Double, 1::Double) gen
    return Neuron (take size rands) (head rands)

data Network = Network [[Neuron]]
data Neuron = Neuron [Double] Double 
