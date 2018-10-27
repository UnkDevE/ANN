module Data
(
    getTrainingData,
    getLabelsFromFile,
    getImagesFromFile
)
where 

import Control.Monad (sequence)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get 
import Data.Word (Word8)

getTrainingData :: String -> String -> IO [([Double], Int)]
getTrainingData imagef labelf = do
    img <- getImagesFromFile imagef
    lab <- getLabelsFromFile labelf
    return $ zip (map (map ((/255) . fromIntegral)) img)
        $ map fromIntegral lab

getLabelsFromFile :: String -> IO [Word8]
getLabelsFromFile file = do
    cont <- BL.readFile file
    return $ runGet readLabels cont

readLabels :: Get [Word8]
readLabels = do
    getWord32be 
    n <- getWord32be
    sequence $ labels (fromIntegral n)

labels :: Int -> [Get Word8]
labels n = do
    getWord8:labels (n-1)

getImagesFromFile :: String -> IO [[Word8]]
getImagesFromFile file = do
    cont <- BL.readFile file
    return $ runGet readImages cont
    
readImages :: Get [[Word8]]
readImages = do
    getWord32be
    n <- getWord32be
    cols <- getWord32be
    rows <- getWord32be
    sequence $ images (fromIntegral n) ((fromIntegral cols) * (fromIntegral rows))

images :: Int -> Int -> [Get [Word8]]
images n pixels = do
    (sequence $ readImage pixels):images (n-1) pixels
    
readImage :: Int -> [Get Word8]
readImage pixels = do
    getWord8:readImage (pixels-1)
    
    



