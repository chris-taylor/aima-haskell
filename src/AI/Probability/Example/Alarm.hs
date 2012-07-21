module AI.Probability.Example.Alarm
    ( alarm
    , AI.Probability.Bayes.enumerationAsk
    , AI.Probability.Bayes.eliminationAsk
    , AI.Probability.Bayes.rejectionAsk
    ) where

import AI.Probability.Bayes

-- |The "alarm" example from AIMA. You can query the network using any of the
--  ask functions. For example, to query the distribution of /Burglary/ given
--  that /JohnCalls/ is true, you would do
--
--  >>> enumerationAsk alarm [("JohnCalls",True)] "Burglary"
--  True   1.6%
--  False  98.4%
--
--  At present, there seems to be a bug, in that the following happens:
--
--  >>> enumerationAsk alarm [("JohnCalls",True)] "JohnCalls"
--  True   5.2%
--  False  94.8%
--
--  whereas I think that a distribution representing certainty should be
--  return instead.
alarm :: BayesNet String
alarm = fromList [ ("Burglary", [], [0.001])
                 , ("Earthquake", [], [0.002])
                 , ("Alarm", ["Burglary","Earthquake"], [0.95,0.94,0.29,0.001])
                 , ("JohnCalls", ["Alarm"], [0.9,0.05])
                 , ("MaryCalls", ["Alarm"], [0.7,0.01]) ]