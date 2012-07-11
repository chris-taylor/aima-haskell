{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module AI.Example.ConstraintSatisfaction where

import Data.Map (Map, (!))

import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.List as L

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

------------
-- Sudoku --
------------

cross :: [a] -> [a] -> [[a]]
cross xs ys = [ [x,y] | x <- xs, y <- ys ]

digits   = "123456789"
rows     = "abcdefghi"
cols     = digits
squares  = cross rows cols
unitlist = [ cross rows c | c <- map return cols ] ++
           [ cross r cols | r <- map return rows ] ++
           [ cross rs cs | rs <- ["abc","def","ghi"], cs <- ["123","456","789"] ]
units    = [ (s, [ u | u <- unitlist, s `elem` u ]) | s <- squares ]
peers    = [ (s, L.delete s $ L.nub $ concat u) | (s,u) <- units ]

data Sudoku v a = Sudoku (Domain String Int) deriving Show

parseGrid :: String -> Sudoku String Int
parseGrid grid =
    Sudoku $ foldr update (mkUniversalMap squares [1..9]) (zip squares grid)
    where
        update (x,y) = if y `elem` digits
            then M.insert x [C.digitToInt y]
            else M.insert x [1..9]

instance CSP Sudoku String Int where

    vars s = squares

    domains (Sudoku dom) = dom

    neighbours s = M.fromList peers

    constraints s x xv y yv = if x `elem` neighbours s ! y
        then xv /= yv
        else True

