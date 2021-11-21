open Tensorast

type sexpr = dimtype * sx
and sx =
  SBinop of sexpr * op * sexpr
| STensor of literal array