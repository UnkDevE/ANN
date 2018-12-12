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
predict (Network neurons) a = foldl' (foldl' (\act (Neuron ws b) -> map (sigmoid . (+ b)) (zipWith (*) ws act))) a neurons 

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
                            return $ updateMiniBatch net (map (map (Neuron) images) ls) eta) net $ chunksOf minibatchSize shuffled
    sgd imagesCont labels (epochs-1) minibatchSize eta newNet

updateMiniBatch :: Network -> [Neuron] -> Double -> Network
updateMiniBatch net@(Network neurons) miniBatch eta =
    flatToNeuron 
        (zip (map (map (subMatrix)) (toWeights net))
            $ foldr1 (\mat (act, err) -> sumMatrix mat $ multiplyMatrices err act) $ map 
                zip (((activations net) . transpose . ((flip shiftL) (repeat 1)) . (flip multiplyMatrix) nm) miniBatch)
                    error)
             $ (map (map (subMatrix)) (toBaises net))
                foldr1 (sumMatrix) map ((flip multiplyMatrix) nm) error
    where nm = (eta / (fromIntegral $ length miniBatch))
          error = foldr1 (sumMatrix) $ map (networkError net) miniBatch 
          
toBaises :: Network -> [[Double]] 
toBaises (Network neurons) = map (map (\(Neuron _ bs) -> bs)) neurons  

shiftL :: [a] -> a -> [a]
shiftL a z = (tail a) ++ z

flatToNeuron :: [([[Double]], [Double])] -> Network
flatToNeuron net = map (\(ws, bs) -> map (\(w,b) -> Neuron w b) $ zip ws bs) 

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
networkError net@(Network neurons) neuron@(Neuron w b) = 
    scanr (\err (ws, act) -> zipWith (*) act $ matrixVectorProduct (transpose ws) err)
        (outputError net neuron) $ zip (toWeights neurons) $ activations net neuron 
        
toWeights :: Network -> [[[Double]]]
toWeights (Network neurons) = map (map (\(Neuron ws _) -> ws)) neurons  

sigmoidPrime z = z * (1 - z)

activations :: Network -> Neuron-> [[Double]]
activations (Network neurons) (Neuron a _) = 
    scanl (foldl' (\act (Neuron ws b) -> map (sigmoid . (+ b)) (zipWith (*) ws act))) a neurons
    
outputError :: Network -> Neuron -> [Double]
outputError net (Neuron w b) =
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
    return $ Neuron (take size rands) (head rands)

data Network = Network [[Neuron]]
data Neuron = Neuron [Double] Double 
