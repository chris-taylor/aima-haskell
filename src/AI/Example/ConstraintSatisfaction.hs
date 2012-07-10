{-# LANGUAGE TypeSynonymInstances #-}

module AI.Example.ConstraintSatisfaction where

import Data.Map (Map, (!))
import qualified Data.Map as M

import AI.Util.Util
import AI.ConstraintSatisfaction

-----------------
-- Example CSP --
-----------------

data ExampleCSP a b = ExampleCSP

exampleCSP :: ExampleCSP Char Int
exampleCSP = ExampleCSP

instance CSP ExampleCSP Char Int where
    vars _ = "XY"
    domains _ = M.fromList [ ('X', [1,2]), ('Y', [1]) ]
    neighbours _ = M.fromList [ ('X',"Y"), ('Y',"X") ]
    constraints _ x xv y yv = xv /= yv

----------------------
-- Map Coloring CSP --
----------------------

data MapColoringCSP v a = MC
    { neighboursMC :: Map String [String]
    , colorsMC :: [Char] }

mapColoringCSP :: [(String,[String])] -> [Char] -> MapColoringCSP String Char
mapColoringCSP nbrs colors = MC (M.fromList nbrs) colors

instance CSP MapColoringCSP String Char where
    vars (MC nbrs _) = M.keys nbrs

    domains csp = mkUniversalMap (vars csp) (colorsMC csp)

    neighbours (MC nbrs _) = nbrs

    constraints csp x xv y yv = if y `elem` neighbours csp ! x
        then xv /= yv
        else True

australia :: MapColoringCSP String Char
australia = mapColoringCSP territories "RGB"
    where
        territories = [ ("SA",  ["WA","NT","Q","NSW","V"])
                      , ("NT",  ["WA","Q","SA"])
                      , ("NSW", ["Q","V","SA"])
                      , ("T",   [])
                      , ("WA",  ["SA","NT"])
                      , ("Q",   ["SA","NT","NSW"])
                      , ("V",   ["SA","NSW"]) ]
