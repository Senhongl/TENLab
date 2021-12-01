open Tensorast

type sdimtype = STensorTup of tensortype * int * int array | SVoidTup

type sexpr = sdimtype * sx
and sx =
  SBinop of sexpr * bop * sexpr
| SUnop of sexpr * uop
| SLunop of luop * sexpr 
| STensor of literal array