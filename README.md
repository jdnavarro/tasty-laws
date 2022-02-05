# Tasty Laws

[![Hackage Version](https://img.shields.io/hackage/v/tasty-laws.svg)](https://hackage.haskell.org/package/tasty-laws)
[![Build Status](https://img.shields.io/travis/jdnavarro/tasty-laws.svg)](https://travis-ci.org/jdnavarro/tasty-laws)
[![Build status](https://github.com/jdnavarro/tasty-laws/actions/workflows/ci.yml/badge.svg)](https://github.com/jdnavarro/tasty-laws/actions/workflows/ci.yml)

Preassembled `tasty` `TestTree`s for property testing the following laws:

- Monoids
- Functors
- Applicatives
- Monads

It uses [`smallcheck-laws`](https://github.com/jdnavarro/smallcheck-laws) under
the hood. If you don't find any runners that suite you, you can use this
package as a reference to implement your own `smallcheck-laws` test runners.

## Contact

Contributions and bug reports are welcome!

Please feel free to contact jdnavarro on the #haskell IRC channel on
irc.freenode.net.
