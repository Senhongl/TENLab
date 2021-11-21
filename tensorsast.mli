open Tensorast

type sdimtype = STensorTup of tensortype * int * int array | SVoidTup

type sexpr = sdimtype * sx
and sx =
  SBinop of sexpr * op * sexpr
| STensor of literal array