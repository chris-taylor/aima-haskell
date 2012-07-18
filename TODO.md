# AIMA Haskell ToDo list

## General

- Complete reorganization. Do we want to separate:
  - core modules
  - interactivity (e.g. playing games)
  - research (e.g. collecting statistics)
  - examples
- Improve Haddock documentation, including section headings.
- Use either System.TimeIt or Criterion for timing calculations.
- Keep making improvements to ProbDist module (or use the PFP module?)

## Utils

- Extensions to table-generating code. For example:
  - Produce a table as a string.
  - Write tables to arbitrary handles.
  - More customisable table layout.
  - Use an external library?
- Improve queueing module to use a more efficient data structure for priority queues.
- Organize Utils module into subsections.

## Search

- Informed/uninformed search
  - More statistics, e.g. time taken.
  - Is it possible to structure backtracking search as a monad?
  - Round out the search functions by including e.g. depth-limited graph search.
  - Fill in missing search functions from AIMA
  - Round effective branching factor to a sensible number of dps.
- Local search
  - Make changes to n-queens problem so that it can be used with simulatedAnnealingsearch.
  - Include genetic algorithm
- Adversarial Search
  - Alpha-beta search that orders nodes according to some heuristic before searching.
  - Stochastic games (using probability monad?)
  - Figure out why alpha/beta search sometimes makes stupid decisions while playingConnect 4.
  - More game examples - e.g. checkers?
- Constraint Satisfaction
  - Wrapper for CSP class to allow statistics to be collected.
  - Examples - word puzzles, scheduling, n queens?

## Logic

- Propositional logic:
  - Local search for SAT
- First-order logic:
  - Parser for logical expressions
  - Theorem prover routines
  - Reduction to normal form

## Probability

- Flesh out functionality for MDPs.
- Partially observed MDPs