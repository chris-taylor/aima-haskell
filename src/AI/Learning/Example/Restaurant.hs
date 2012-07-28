module AI.Learning.Example.Restaurant where

import AI.Learning.DecisionTree

data Patrons = Empty | Some | Full deriving (Show,Eq,Ord,Enum,Bounded)
data Price = Cheap | Medium | Expensive deriving (Show,Eq,Ord,Enum,Bounded)
data Type = French | Thai | Burger | Italian deriving (Show,Eq,Ord,Enum,Bounded)
data Wait = None | Short | Med | Long deriving (Show,Eq,Ord,Enum,Bounded)

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
    wait :: Wait,       -- what is the wait?
    willWait :: Bool    -- will you wait?
} deriving (Eq,Ord)

x1  = Restaurant True False False True Some Expensive False True French None True
x2  = Restaurant True False False True Full Cheap False False Thai Med False
x3  = Restaurant False True False False Some Cheap False False Burger None True
x4  = Restaurant True False True True Full Cheap True False Thai Short True
x5  = Restaurant True False True False Full Expensive False True French Long False
x6  = Restaurant False True False True Some Medium True True Italian None True
x7  = Restaurant False True False False Empty Cheap True False Burger None False
x8  = Restaurant False False False True Some Medium True True Thai None True
x9  = Restaurant False True True False Full Cheap True False Burger Long False
x10 = Restaurant True True True True Full Expensive False True Italian Short False
x11 = Restaurant False False False False Empty Cheap False False Thai None False
x12 = Restaurant True True True True Full Cheap False False Burger Med True

restaurants = [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x2]

atts = [ att alt "Alternative"
       , att bar "Bar"
       , att fri "Friday"
       , att hun "Hungry"
       , att pat "Patrons"
       , att price "Price"
       , att rain "Raining"
       , att res "Reservation"
       , att food "Food"
       , att wait "Wait" ]

actualTree = do
  patrons <- attribute pat "Patrons"
  case patrons of
    Empty -> return False
    Some  -> return True
    Full  -> do
      time <- attribute wait "WaitTime"
      case time of
        None  -> return True
        Short -> do
          hungry <- attribute hun "Hungry"
          if not hungry
            then return True
            else do
              alternative <- attribute alt "Alternative"
              if not alternative
                then return True
                else do
                  raining <- attribute rain "Rain"
                  return (if raining then True else False)
        Med   -> do
          alternative <- attribute alt "Alternative"
          if not alternative
            then do
              reservation <- attribute res "Reservation"
              if reservation
                then return True
                else do
                  hasBar <- attribute bar "Bar"
                  return (if hasBar then True else False)
            else do
              friday <- attribute fri "Fri/Sat"
              return (if friday then True else False)
        Long  -> return False

fittedTree = fitTree willWait atts restaurants


