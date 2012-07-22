import AI.Probability.Example.Grass
import AI.Probability.Bayes
import System.Environment (getArgs)
  
main = do 
    args <- getArgs
    let n = read (head args) :: Int
    test <- likelihoodWeighting n grass [] "Rain"  
    print test
