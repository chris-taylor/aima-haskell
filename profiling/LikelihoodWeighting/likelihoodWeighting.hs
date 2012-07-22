import AI.Probability.Example.Alarm
import AI.Probability.Bayes
import AI.Util.ProbDist

main :: IO ()
main = do
    putStrLn "----------"

    let d1 = enumerationAsk alarm fixed x
    putInfo d1

    d2 <- likelihoodWeighting 100000 alarm fixed x
    putInfo d2

fixed :: [(String,Bool)]
fixed = [("JohnCalls",True),("MaryCalls",True)]

x :: String
x = "Burglary"

putInfo :: Show a => Dist a -> IO ()
putInfo d = do
    putStrLn $ show d
    putStrLn $ "----------"
