{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module AI.Example.ConstraintSatisfaction where

import Data.Map (Map, (!))
import qualified Data.Map as M

import AI.Util.Graph (Graph)
import AI.Util.Util
import AI.ConstraintSatisfaction

import qualified AI.Util.Graph as G

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

data MapColoringCSP v a = MCP
    { neighboursMC :: Graph String
    , colorsMC :: [Char] } deriving (Show)

instance CSP MapColoringCSP String Char where
    vars (MCP nbrs _) = M.keys nbrs

    domains csp = mkUniversalMap (vars csp) (colorsMC csp)

    neighbours (MCP nbrs _) = nbrs

    constraints csp x xv y yv = if y `elem` neighbours csp ! x
        then xv /= yv
        else True

australia :: MapColoringCSP String Char
australia = MCP territories "RGB"
    where
        territories = G.toGraph $
            [ ("SA",  ["WA","NT","Q","NSW","V"])
            , ("NT",  ["WA","Q","SA"])
            , ("NSW", ["Q","V","SA"])
            , ("T",   [])
            , ("WA",  ["SA","NT"])
            , ("Q",   ["SA","NT","NSW"])
            , ("V",   ["SA","NSW"]) ]

usa :: MapColoringCSP String Char
usa = MCP states "RGBY"
    where states = G.parseGraph
            "WA: OR ID; OR: ID NV CA; CA: NV AZ; NV: ID UT AZ; ID: MT WY UT;\
            \UT: WY CO AZ; MT: ND SD WY; WY: SD NE CO; CO: NE KA OK NM; NM: OK TX;\
            \ND: MN SD; SD: MN IA NE; NE: IA MO KA; KA: MO OK; OK: MO AR TX;\
            \TX: AR LA; MN: WI IA; IA: WI IL MO; MO: IL KY TN AR; AR: MS TN LA;\
            \LA: MS; WI: MI IL; IL: IN; IN: KY; MS: TN AL; AL: TN GA FL; MI: OH;\
            \OH: PA WV KY; KY: WV VA TN; TN: VA NC GA; GA: NC SC FL;\
            \PA: NY NJ DE MD WV; WV: MD VA; VA: MD DC NC; NC: SC; NY: VT MA CA NJ;\
            \NJ: DE; DE: MD; MD: DC; VT: NH MA; MA: NH RI CT; CT: RI; ME: NH;\
            \HI: ; AK: "
