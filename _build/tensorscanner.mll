{ open Tensorparser }

let digit = ['0' - '9']
let digits = digit+
let exponent = ['e' 'E'] ['+' '-']? digits

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| ',' { C }
| '[' { L }
| ']' { R }
| '(' { LP }
| ')' { RP }
| "int" { INT }
| "float" { FLOAT }
| digits as lit { INT_LITERAL(int_of_string lit) }
| (digits '.' digit* exponent? | digits exponent | '.' digits exponent?) as lit { FLOAT_LITERAL(float_of_string lit) }
| eof { EOF }
