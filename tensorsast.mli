open Tensorast

type sdimtype = STensorTup of tensortype * int * int array | SVoidTup

type sexpr = sdimtype * sx
and sx =
  SBinop of sexpr * bop * sexpr
| SUnop of sexpr * uop
| STensor of literal array