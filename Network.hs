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
predictHighest net a = snd $ foldl (\p@(acc, _) n@(a, _) -> if acc < a then n else p) (0, 0) $ zip (predict net a) [0..]

predict :: Network -> [Double] -> [Double]
predict (Network weights baises) a = 
    foldl' (\act (ws, bs) -> zipWith ((+) . sigmoid) bs $ matrixVectorProduct ws act) a
        $ zip weights baises

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
                $ map (\weight -> (foldr (\(err, act) nxt -> sumMatrix nxt $ multiplyMatrices act err) 
                        weight
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

networkError :: Network -> ([Double], Int) -> [[[Double]]]
networkError net@(Network ws bs) ex@(x, y) = scanl (\error (act, tws) ->
            matrixVectorProduct act $ matrixVectorProduct tws error)
        (outputError net ex) $ zip (map (map (sigmoidPrime)) $ activations net x) $ map (transpose) ws

sigmoidPrime z = z * (1 - z)

activations :: Network -> [Double] -> [[Double]]
activations (Network weights baises) a = 
    scanl (\act (ws, bs) -> zipWith ((+) . sigmoid) bs $ matrixVectorProduct ws act) a
        $ zip weights baises

outputError :: Network -> ([Double], Int) -> [Double]
outputError net (x, y) =
    zipWith (*) (zipWith (-) (predict net x) $ actual y) $ map (sigmoidPrime) $ predict net x
    
actual :: (Num a) => Int -> [a] 
actual n = [if x == 0 then 1 else 0 | x <- [n, (n-1)..]]

emptyNetwork :: [Int] -> IO Network
emptyNetwork sizes = do
    gen <- getStdGen
    let rands = randomRs (0::Double, 1::Double) gen
    return $ Network [w | neuronSizes <- tail sizes, w <- take neuronSizes $ repeat [ x | weightSizes <- init sizes, x <- [take weightSizes rands]]]
                [w | neuronSizes <- tail sizes, w <- [rands]] 
        
data Network = Network [[[Double]]] [[Double]]
