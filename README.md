# RootGame

A simple guessing game to practice Haskell and functional programming in general. Try and find a root of a function by trying different inputs and observing their outputs!

Currently supports Int -> Int functions, might be extended further in the future.

Example method to run when loaded into ghci:

> rootgame (\x -> x^2 - 9) 10