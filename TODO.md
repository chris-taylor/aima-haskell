# AIMA Haskell ToDo list

## General

- Reorganization. Keep core algorithms in their current place, and move examples and interative code to separate modules.

##Search

- More statistics, e.g. effective branching factor
- Is it possible to structure backtracking search as a monad?

## Games

- Depth-limited and iterative deepening minimax search
- Alpha-beta search that orders nodes according to some heuristic before searching
- GameIO data type that collects statistics as the game is played
- Stochastic games (using probability monad?)