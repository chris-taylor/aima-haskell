module AI.Probability.Example.Grass
    ( grass
    , AI.Probability.Bayes.enumerationAsk
    , AI.Probability.Bayes.eliminationAsk
    , AI.Probability.Bayes.rejectionAsk
    , AI.Probability.Bayes.likelihoodWeighting
    ) where

import AI.Probability.Bayes

-- |The "grass" example from AIMA. You can query the network using any of the
--  ask functions. For example, to query the distribution of /Rain/ given
--  that /GrassWet/ is true, you would do
--
--  >>> enumerationAsk alarm [("GrassWet",True)] "Rain"
--  True   35.8%
--  False  64.2%
--
--  The same bug as in the alarm example also occurs here.
grass :: BayesNet String
grass = fromList [ ("Rain", [],   [0.2])
                 , ("Sprinkler", ["Rain"], [0.01, 0.4])
                 , ("GrassWet", ["Sprinkler","Rain"], [0.99, 0.9, 0.8, 0]) ]