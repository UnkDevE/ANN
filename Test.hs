module Test 
(
    evaluate
)

import TrainingData

evaluate :: Network -> Double 
evaluate net = 
    loadTestData "trainingData/t10k-images-idx3-ubyte" "trainingData/t10k-labels-idx1-ubyte"
    predictHighest image 

loadTestData :: String -> String -> 
loadTestData imagesFile labelsFile = do
    getLabelsFromFile labelsFile 
