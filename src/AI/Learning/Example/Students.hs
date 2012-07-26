module AI.Learning.Example.Students where

import AI.Learning.DecisionTree

data Student = Student {
    firstLastYear :: Bool,
    male :: Bool,
    worksHard :: Bool,
    drinks :: Bool,
    firstThisYear :: Bool } deriving (Eq,Ord,Show)

-- |The training set.
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
atts = [
    Att firstLastYear "firstLastYear",
    Att male "male",
    Att worksHard "worksHard",
    Att drinks "drinks" ]

target = firstThisYear

tree = fmap (map target . snd) $
        prune (\(a,as) -> entropy as == 0) $
        runSplitter (minSplit $ sumEntropy target) (atts,students)