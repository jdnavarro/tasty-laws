# Change Log
All notable changes to this project will be documented in this file. This file
follows the formatting recommendations from [Keep a
CHANGELOG](http://keepachangelog.com/). This project adheres to [Semantic
Versioning](http://semver.org/).

## [0.2] - 2015-09-04
### Removed
- `smallcheck` specific modules from
  [`smallcheck-laws-0.1`](https://hackage.haskell.org/package/smallcheck-laws-0.1).
  This package now contains `Tasty` specific modules.

### Changed
- Simplify module hierarchy: `Test.Tasty.SmallCheck.Laws` -> `Test.Tasty.Laws`

[0.2]: https://github.com/jdnavarro/tasty-laws/compare/bf1caa5...v0.2
