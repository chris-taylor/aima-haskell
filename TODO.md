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
  - Write tables to arbitrary handles.
  - More customisable table layout.
  - Use an external library?
- Improve queueing module to use a more efficient data structure for priority queues.
- Rewrite the Util.Array module to actually use arrays!
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
  - Truth-table SAT
  - Local search for SAT
  - Backward chaining
- First-order logic:
  - Backward chaining
  - Reduction to normal form

## Probability

- Flesh out functionality for MDPs.
- Partially observed MDPs
- Bayes Net uses lists rather than arrays to store the conditional probability table. This is probably inefficient - profile it and check!
- Markov chain (Gibbs sampling) routines for Bayes Net
- Function to compute children of a node in a Bayes Net
- Function to compute markov blanket of a node in a Bayes Net

## Learning

- Prune decision trees using
  - Max number of nodes
  - Significance test at nodes
- More functions to auto-fit decision trees
- Decision tree demos are really slow - can they be optimized?
- Handle continuous attributes
- Compute precision, recall and f-statistic
- Function to compare multiple classifiers
- More examples - test random forest vs. pruned decision trees
- Linear classifiers
- Naive Bayes