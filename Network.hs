module Network 
(
    sgd,
    networkError,
    updateMiniBatch,
    outputError,
    emptyNetwork,
    transpose,
    unflatten,
    predict,
    predictHighest,
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

predictHighest :: Network -> [Double] -> Int
predictHighest net a = snd $ foldl (\p@(acc, _) n@(a, _) -> if acc < a then n else p) (0, 0) $ zip (predict net a) [0..]

predict :: Network -> [Double] -> [Double]
predict (Network xs) a =  foldl' (\i layer -> 
    foldl' (\acc (w, b) -> map (\act -> sigmoid $ (dot w a) + b) acc) i layer) [] xs

dot => (Num a) -> [a] -> [a] -> a
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
updateMiniBatch net@(Network xs) miniBatch eta =
            Network $ zipWith (zip) (zipWith (subMatrix) (map (fst $ unzip) xs)
                    (map (flip multiplyMatrix) (eta / fromIntegral $ length miniBatch) 
                        (foldr (\acc (nxt, act) -> sumMatrix acc $ multiplyMatrices act nxt) [] 
                            zip (map (networkError net) miniBatch) (map (activations net) (fst $ unzip $ miniBatch)))))
                $ zipWith (zipWith (-) $ zipWith (*) (repeat (eta / fromIntegral $ length miniBatch))) 
                    $ foldr (\acc nxt -> zipWith (+) acc nxt) (repeat 0) $ map (networkError net) miniBatch 


subMatrix :: (Num a) => [[a]] -> [[a]] -> [[a]]
subMatrix xxs yys = map (map (\(x,y) -> zipWith (-) x y) $ zip xxs yys

sumMatrix :: (Num a) => [[a]] -> [[a]] -> [[a]]
sumMatrix xxs yys = map (map (\(x,y) -> zipWith (+) x y) $ zip xxs yys

multiplyMatrix :: (Num a) => [[a]] -> a -> [[a]]
multiplyMatrix xxs y = map (map (* y)) xxs

multiplyMatrices :: (Num a) => [[a]] -> [[a]] -> [[a]]
multiplyMatrices xxs yys = 
    map (\xs -> map (dot xs) $ transpose yys) xxs

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

networkError :: Network -> ([Double], Int) -> [[Double]]
networkError net@(Network xs) ex@(x, y) = scanl (\error (act, tws) ->
            zipWith (*) (matrixVectorProduct tws error) $ map (sigmoidPrime) act)
        (outputError net ex) $ zip (activations net x) $ map (transpose $ (fst $ unzip ys)) xs

matrixVectorProduct (Num a) => [[a]] -> [a] -> [a]
matrixVectorProduct xxs xs = map (dot xs) xxs

activations :: Network -> [Double] -> [[Double]]
activations (Network xs) a =  scanr 
    (\i layer -> foldl' (\acc (w, b) -> map (\act -> sigmoid $ (dot w a) + b) acc) i layer) [] xs

sigmoidPrime z = z * (1 - z)

outputError :: Network -> ([Double], Int) -> [Double]
outputError net (x, y) = 
    zipWith (*) (zipWith (-) (predict net x) y) $ map (sigmoidPrime) $ predict net x
    
actual :: (Num a) => Int -> [a] 
actual n = [if x == 0 then 1 else 0 | x <- [n, (n-1)..]]

makeEmptyNetwork :: [Int] -> IO Network
makeEmptyNetwork sizes = do
    gen <- getStdGen
    let rands = randomRs (0::Double, 1::Double) gen
    return $ Network 
        map (\(i, t) -> zip (take t $ repeat $ take i rands) (take t rands)) $ zip (init sizes) (tail sizes)
        
data Network = Network [[([Double], Double)]] 
