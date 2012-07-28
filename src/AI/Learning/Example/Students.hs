module AI.Learning.Example.Students where

import qualified Data.Map as M
import AI.Learning.DecisionTree

data Student = Student {
    firstLastYear :: Bool,
    male :: Bool,
    worksHard :: Bool,
    drinks :: Bool,
    firstThisYear :: Bool } deriving (Eq,Ord,Show)

students = [richard,alan,alison,jeff,gail,simon]

richard = Student True True False True True
alan    = Student True True True False True
alison  = Student False False True False True
jeff    = Student False True False True False
gail    = Student True False True True True
simon   = Student False True True True False

matthew = Student False True False True True
mary    = Student False False True True False

-- |Attributes.
atts = [ Att (fromEnum . firstLastYear) "firstLastYear"
       , Att (fromEnum . male) "male"
       , Att (fromEnum . worksHard) "worksHard"
       , Att (fromEnum . drinks) "drinks" ]


tree :: DTree Student Bool
tree = do
  a <- attribute firstLastYear "firstLastYear"
  if a
    then return True
    else do
      b <- attribute male "male"
      if b
        then return False
        else do
          c <- attribute worksHard "worksHard"
          if c
            then return True
            else do
              d <- attribute drinks "drinks"
              return (if d then False else True)



--tree = fitTree firstThisYear atts students
