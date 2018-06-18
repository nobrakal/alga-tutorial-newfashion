# alga-tutorial

This will teach you the basis of [algebraic-graphs](https://github.com/snowleopard/alga). It works like GHCI, so you can enter real Haskell code to be interpreted.

## Use
At the begining you have access only to some commands:
```
 Type "help" to show this message 
 Type "quit" to quit 
 Type "module i" to go to the module n° i. Modules available:  
  0. Basics 
  1. The Num instance 
  2. Mathematical laws 
  3. Useful functions and instances 
  4. An example: a network
```

A __Module__ is a set of exercices focused on one topic. They are ordered, try to do them in order.
An exercise is called a __Submodule__. It will ask you to enter some code, and evaluate your answer.

Once you loaded a module, you have can enter Haskell code AND all the previous commands AND some new commands:
```
 Type "clue" to have a clue on the current goal
 Type "skip" to show the solution and skip
 Type "submodule i" to go to the submodule n° i.
```

## Build and run

Please use this tutorial either

### Using cabal
```Bash
$ cabal build
$ dist/build/tutorial/tutorial
```

### Using stack
```Bash
$ stack build
$ stack exec tutorial
```

This method can make the starting time longer (about 2/3s)

## History
History is kept in ".history"
