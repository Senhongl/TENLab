type token =
  | NEWLINE
  | EOF
  | PLUS
  | SUBTRACT
  | MULTIPLICATION
  | DOT_MULTIPLICATION
  | DIVIDE
  | POWER
  | DOT_POWER
  | TRANSPOSE
  | MOD
  | FLOOR_DIVIDE
  | NEG
  | IS_EQUAL
  | IS_GEQ
  | IS_GT
  | IS_LEQ
  | IS_LT
  | IS_NOT_EQUAL
  | AND
  | OR
  | NOT
  | LEFT_PARENTHESIS
  | RIGHT_PARENTHESIS
  | LEFT_CURLY_BRACKET
  | RIGHT_CURLY_BRACKET
  | LEFT_SQUARE_BRACKET
  | RIGHT_SQUARE_BRACKET
  | COMMA
  | COLONS
  | ASSIGNMENT
  | IF
  | ELIF
  | ELSE
  | FOR
  | WHILE
  | IN
  | CONTINUE
  | BREAK
  | RETURN
  | EXIT
  | DEFINE
  | INT
  | FLOAT
  | STRING
  | INT_LITERAL of (int)
  | STRING_LITERAL of (string)
  | FLOAT_LITERAL of (float)
  | IDENTIFIER of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.stmt list
