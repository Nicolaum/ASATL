# ASATL

A Statical Analyser for a Tiny Language in Coq... Maybe exportet at some point.

It is a Coq file, so you know what to do, else look at coq.org.

## IDEA

The idea behind the project is to see if we can define languages, where

if all inputs verified then proper result else throw an error.

Or more formalized:

forall i : inputs,
  program (valid i)   -> valid output
  /\
  program (inValid i) -> output error.
