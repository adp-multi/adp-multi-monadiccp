adp-multi-monadiccp [![Build Status](https://secure.travis-ci.org/adp-multi/adp-multi-monadiccp.png?branch=master)](http://travis-ci.org/adp-multi/adp-multi-monadiccp)
===================

This library contains an alternative implementation of the `ADP.Multi.Rewrite.*`
modules in [adp-multi](https://github.com/adp-multi/adp-multi).
It uses the same model (`ADP.Multi.Rewrite.Model`) for rewriting functions,
but implements a different subword construction algorithm using the
`OvertonFD` constraint solver implemented within the constraint programming
framework [monadiccp](http://hackage.haskell.org/package/monadiccp).

It was developed as an experiment to reduce code size and eventually be able
to generically construct subwords for any dimension. At the moment, it is slower
than the standard implementation and supports one- and two-dimensional nonterminals
only.

How to use
----------

1. Check out the git repository
2. Run `cabal install` inside the checked out folder
3. Add `adp-multi-monadiccp` as a dependency to your cabal project
4. Instead of importing `ADP.Multi.Rewriting.All` in your grammar files, 
   import `ADP.Multi.Constraint.All`
   
The library is also published on [Hackage](http://hackage.haskell.org/package/adp-multi-monadiccp).