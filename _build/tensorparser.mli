type token =
  | L
  | C
  | R
  | LP
  | RP
  | EOF
  | INT
  | FLOAT
  | INT_LITERAL of (int)
  | FLOAT_LITERAL of (float)

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tensorast.expr
