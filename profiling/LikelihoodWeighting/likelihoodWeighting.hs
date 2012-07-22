import AI.Probability.Example.Alarm
import AI.Util.ProbDist

n :: Int
n = 100000

fixed :: [(String,Bool)]
fixed = [("JohnCalls",True),("MaryCalls",True)]

x :: String
x = "Burglary"

main :: IO ()
main = do
    putStrLn "----------"

    let d1 = enumerationAsk alarm fixed x
    putInfo d1

    d2 <- likelihoodWeighting n alarm fixed x
    putInfo d2

putInfo :: Show a => Dist a -> IO ()
putInfo d = do
    putStrLn $ show d
    putStrLn $ "----------"
