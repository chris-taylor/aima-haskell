# AIMA Haskell ToDo list

## General

- Reorganization. Keep core algorithms in their current place, and move examples and interactive code to separate modules.
- Improve Haddock documentation, including section headings.
- Use either System.TimeIt or Criterion for timing calculations.

## Utils

- Extensions to table-generating code. For example:
  - Produce a table as a string
  - Write tables to arbitrary handles
  - More customisable table layout
  - Use an external library?
- Improve queueing module to use a more efficient data structure

##Search

- More statistics, e.g. effective branching factor, time taken
- Is it possible to structure backtracking search as a monad?

## Games

- Depth-limited and iterative deepening minimax search
- Alpha-beta search that orders nodes according to some heuristic before searching
- GameIO data type that collects statistics as the game is played, e.g. number of nodes expanded, number of times heuristic function is called, time taken.
- Move ordering function - possibly make this a method of the Game class, with a default implementation of
    orderMoves :: [a] -> [a]
    orderMoves = id
- Stochastic games (using probability monad?)

## Probability
- Documentation