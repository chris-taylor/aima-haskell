module AI.Learning.Example.Restaurant where

import AI.Learning.DecisionTree

data Patrons = None | Some | Full deriving (Show,Eq,Ord)
data Price = Cheap | Medium | Expensive deriving (Show,Eq,Ord)
data Type = French | Thai | Burger | Italian deriving (Show,Eq,Ord)

data Restaurant = Restaurant {
    alt :: Bool,        -- is there an alternative?
    bar :: Bool,        -- is there a bar?
    fri :: Bool,        -- is it a friday?
    hun :: Bool,        -- are you hungry?
    pat :: Patrons,     -- how many patrons are there?
    price :: Price,     -- how cheap is it?
    rain :: Bool,       -- is it raining?
    res :: Bool,        -- do you have a reservation?
    food :: Type,       -- what type of food is it?
    wait :: Int,        -- what is the wait?
    willWait :: Bool    -- will you wait?
} deriving (Eq,Ord)

x1  = Restaurant True False False True Some Expensive False True French 0 True
x2  = Restaurant True False False True Full Cheap False False Thai 30 False
x3  = Restaurant False True False False Some Cheap False False Burger 0 True
x4  = Restaurant True False True True Full Cheap True False Thai 10 True
x5  = Restaurant True False True False Full Expensive False True French 60 False
x6  = Restaurant False True False True Some Medium True True Italian 0 True
x7  = Restaurant False True False False None Cheap True False Burger 0 False
x8  = Restaurant False False False True Some Medium True True Thai 0 True
x9  = Restaurant False True True False Full Cheap True False Burger 60 False
x10 = Restaurant True True True True Full Expensive False True Italian 10 False
x11 = Restaurant False False False False None Cheap False False Thai 0 False
x12 = Restaurant True True True True Full Cheap False False Burger 30 True

restaurants = [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x2]

atts = [ Att alt "Alternative"
       , Att bar "Bar"
       , Att fri "Friday"
       , Att hun "Hungry"
       , Att ((==None) . pat) "NoPatrons"
       , Att ((==Full) . pat) "Full"
       , Att ((==Cheap) . price) "Cheap"
       , Att ((==Expensive) . price) "Expensive"
       , Att rain "Raining"
       , Att res "Reservation" ]
       --, Att food "Cuisine"
       --, Att wait "WaitTime" ]

target = willWait

tree = prune (\as -> entropy as == 0) $
        fmap (map target . snd) $
        runSplitter (minSplit $ sumEntropy target) (atts,restaurants)


