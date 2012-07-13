# AIMA Haskell ToDo list

## General

- Complete reorganization. Do we want to separate:
  - core modules
  - interactivity (e.g. playing games)
  - research (e.g. collecting statistics)
  - examples
- Improve Haddock documentation, including section headings.
- Use either System.TimeIt or Criterion for timing calculations.

## Utils

- Extensions to table-generating code. For example:
  - Produce a table as a string.
  - Write tables to arbitrary handles.
  - More customisable table layout.
  - Use an external library?
- Improve queueing module to use a more efficient data structure.
- Organize Utils module into subsections.

## Search

- More statistics, e.g. time taken.
- Is it possible to structure backtracking search as a monad?
- Make changes to n-queens problem so that it can be used with simulatedAnnealing search.
- Round effective branching factor to a sensible number of dps.

## Games

- Alpha-beta search that orders nodes according to some heuristic before searching.
- Stochastic games (using probability monad?)
- Figure out why alpha/beta search sometimes makes stupid decisions while playing Connect 4.

## Constraint Satisfaction

- Wrapper for CSP class to allow statistics to be collected.
- Examples - word puzzles, scheduling?

## Probability

- Flesh out functionality.