{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module AI.Agents where

import System.IO.Unsafe

-------------
-- Objects --
-------------

-- |The Object class represents any physical object that can appear in an
--  Environment. You should create instances of Object to get the objects you
--  want.
class Show o => Object o where
    -- |Objects that are alive should return 'True'
    isAlive :: o -> Bool

------------
-- Agents --
------------

-- |An Agent is a type with one method, 'program', which returns a function
--  of one argument, percept -> action. An agent program that needs a model of
--  the world (and of the agent itself) will have to build and maintain its
--  own model.
class Object (agent p a) => Agent agent p a where
    -- |Given a percept, return an appropriate acion.
    program :: agent p a -> p -> a

-- |A wrapper for an agent that will print its percepts and actions. This will
--  let you see what the agent is doing in the environment.
data TraceAgent agent p a = TraceAgent { getAgent :: agent p a } deriving Show

-- |Make a TraceAgent into an instance of Object by wrapping 'isAlive'.
instance Object (agent p a) => Object (TraceAgent agent p a) where
    isAlive (TraceAgent agent) = isAlive agent

-- |Make a TraceAgent into an instance of Agent by wrapping 'program'.
instance (Agent agent p a, Show p, Show a) => Agent (TraceAgent agent) p a where
    program (TraceAgent agent) p = unsafePerformIO $ do
        let a = program agent p
        putStrLn $
            show agent ++ " perceives " ++ show p ++ " and does " ++ show a
        return a

------------------
-- Environments --
------------------

-- |The Environment class contains is used to maintain a collection of objects
--  and agents.
class (Object o, Agent agent p a) => Environment e o agent p a where
    -- |Return a list of objects in the current environment.
    objects :: e o agent p a -> [o]

    -- |Return a list of agents in the current environment.
    agents :: e o agent p a -> [agent p a]

    -- |Return the percent that the agent sees at this point.
    percept :: e o agent p a -> agent p a -> p

    -- |Modify the environment to reflect an agent taking an action.
    execAction :: e o agent p a -> agent p a -> a -> e o agent p a

    -- |Is the execution over? By default, we are done when we can't find a
    --  live agent.
    isDone :: e o agent p a -> Bool
    isDone env = not $ any isAlive $ agents env

