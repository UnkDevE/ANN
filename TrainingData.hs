module TrainingData
(
    loadBatch,
    loadImage,
    getLabelsFromFile
)
where 

import Control.Monad (replicateM, sequence)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get 
import Data.Word (Word8, Word32)
import Data.List.Split (chunksOf)

getLabelsFromFile :: String -> IO [Word8]
getLabelsFromFile file = do
    cont <- BL.readFile file
    return $ runGet readLabels cont

readLabels :: Get [Word8]
readLabels = do
    getWord32be 
    n <- getWord32be
    replicateM (fromIntegral n) getWord8

loadBatch :: String -> [Int] -> IO [[Double]]
loadBatch file ns = sequence $ map (loadImage file) ns 

loadImage :: String -> Int -> IO [Double]
loadImage file n  = readImage file n >>= prepareImage

prepareImage :: [Word8] -> IO [Double]
prepareImage image = return $ map (\n -> fromIntegral n / 255) image 

readImage :: String -> Int -> IO [Word8]
readImage file n = do
    cont <- BL.readFile file  
    let pixels = runGet readImagesHeader cont 
    let bs = BL.drop (fromIntegral $ pixels*n) cont
    return $ runGet (replicateM pixels getWord8) bs

readImagesHeader :: Get Int
readImagesHeader = do 
    getWord32be
    n <- getWord32be
    cols <- getWord32be
    rows <- getWord32be
    return $ fromIntegral (cols * rows)

