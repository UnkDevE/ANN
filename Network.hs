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

matrixVectorProduct :: [[Double]] -> [Double] -> [Double]
matrixVectorProduct xxs xs = map (dot xs) xxs

dot :: (Num a) => [a] -> [a] -> a
dot xs ys = sum $ zipWith (*) xs ys 

sigmoid z = 1 / (1 + exp (-z))

data TrainData = TrainData [Double] Int 

sgd :: BL.ByteString -> [Int] -> Int -> Int -> Double -> Network -> IO Network
sgd imagesCont labels 0 minibatchSize eta net = return net
sgd imagesCont labels epochs minibatchSize eta net = do
    gen <- getStdGen
    let shuffled = shuffle' (zip labels [0..]) (length labels) gen
    newNet <- foldM (\net batch -> do 
                            let (ls, ns) = unzip batch
                            images <- loadBatch imagesCont ns 
                            return $ updateMiniBatch net 
                                (zipWith ($) (map (TrainData) images) ls) eta) 
                net$ chunksOf minibatchSize shuffled
    sgd imagesCont labels (epochs-1) minibatchSize eta newNet

updateMiniBatch :: Network -> [TrainData] -> Double -> Network
updateMiniBatch net@(Network neurons) miniBatch eta =
    flatToNeuron $ zip
        (zipWith ($) (map (subMatrix) $ toWeights net)
            $ scanr (\(act, err) mat -> sumMatrix mat $ multiplyMatrices act err) 
                (repeat (repeat (0::Double)))
                $ zip (((map (activations net)) . 
                        transpose . 
                        ((flip shiftL) (repeat (1::Double))) . 
                        ((flip multiplyMatrix) nm)) weights)
                    (repeat error))
            $ subMatrix (toBaises net)
                $ ((flip multiplyMatrix) nm) error 
    where nm = (eta / (fromIntegral $ length miniBatch))
          error = (foldr1 (sumMatrix)) $ map (networkError net) miniBatch 
          weights = (\xs -> map (\(TrainData ws _) -> ws) xs) miniBatch
          
toBaises :: Network -> [[Double]] 
toBaises (Network neurons) = map (map (\(Neuron _ bs) -> bs)) neurons  

shiftL :: [a] -> a -> [a]
shiftL a z = (tail a) ++ [z]

flatToNeuron :: [([[Double]], [Double])] -> Network
flatToNeuron net = Network $ map (\(ws, bs) -> map (\(w,b) -> Neuron w b) $ zip ws bs) net 

subMatrix :: [[Double]] -> [[Double]] -> [[Double]]
subMatrix xxs yys = map (\(x,y) -> zipWith (-) x y) $ zip xxs yys

sumMatrix :: [[Double]] -> [[Double]] -> [[Double]]
sumMatrix xxs yys = map (\(x,y) -> zipWith (+) x y) $ zip xxs yys

multiplyMatrix :: [[Double]] -> Double -> [[Double]]
multiplyMatrix xxs y = map (map (* y)) xxs

multiplyMatrices :: [[Double]] -> [[Double]] -> [[Double]]
multiplyMatrices xxs yys = 
    map (\xs -> map (dot xs) $ transpose yys) xxs

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

networkError :: Network -> TrainData -> [[Double]]
networkError net@(Network neurons) neuron@(TrainData w b) = 
    foldr (\(ws, act) err -> havardProduct act $ multiplyMatrices (transpose ws) err) 
        (outputError net neuron) $ zip (toWeights net) $ (repeat $ activations net w)

havardProduct :: [[Double]] -> [[Double]] -> [[Double]]
havardProduct xxs yys =
    map (\(xs, ys) -> zipWith (*) xs ys) $ zip xxs yys

toWeights :: Network -> [[[Double]]]
toWeights (Network neurons) = map (map (\(Neuron ws _) -> ws)) neurons  

sigmoidPrime :: Double -> Double
sigmoidPrime z = z * (1 - z)
    
outputError :: Network -> TrainData -> [[Double]]
outputError net (TrainData w a) =
    map (map (sigmoidPrime) . (zipWith (*) $ zipWith (-) (predict net w) (actual a))) $ activations net w
    
activations :: Network -> [Double] -> [[Double]]
activations (Network neurons) a = 
    scanl (foldl' (\act (Neuron ws b) -> map (sigmoid . (+ b)) (zipWith (*) ws act))) a neurons

actual :: Int -> [Double] 
actual n = [if x == 0 then 1 else 0 | x <- [n, (n-1)..]]

emptyNetwork :: [Int] -> IO Network
emptyNetwork sizes = 
    fmap (Network) $ sequence $ map (sequence) $
        map (\(e, t) -> replicate t e) $ zip (map (emptyNeuron) (init sizes)) $ tail sizes

emptyNeuron :: Int -> IO Neuron 
emptyNeuron size = do
    gen <- getStdGen 
    let rands = randomRs (0::Double, 1::Double) gen
    return $ Neuron (take size rands) (head rands)

data Network = Network [[Neuron]]
data Neuron = Neuron [Double] Double 
