{ open Tensorparser }

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| ',' { C }
| '[' { L }
| ']' { R }
| ['0'-'9']+ as num { NUM(int_of_string num) }
| eof { EOF }
