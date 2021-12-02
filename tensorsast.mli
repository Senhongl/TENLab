open Tensorast

type sdimtype = STensorTup of tensortype * int * int array | SVoidTup

type index = int * int array * literal array

type sasexpr = 
  Ident of string
| Idind of string * (int * index list)

type sexpr = sdimtype * sx
and sx =
  SBinop of sexpr * op * sexpr
| STensor of literal array
| SVtensor of tensor
| SASexpr of sasexpr