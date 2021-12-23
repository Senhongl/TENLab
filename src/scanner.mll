{
        open Parser
}

let digit = ['0' - '9']
let digits = digit+
let exponent = ['e' 'E'] ['+' '-']? digits

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| [';'] { SEP }

(* comments *)
| '#' { comment_line lexbuf }
| "'''" { comment_block lexbuf }

(* arithmetic operators *)
| '+' { PLUS }
| '-' { SUBTRACT }
| '*' { MULTIPLICATION }
| ".*" { DOT_MULTIPLICATION }
| '/' { DIVIDE }
| '^' { POWER }
| ".^" { DOT_POWER }
| "'" { TRANSPOSE }
| "%" { MOD }
| "//" { FLOOR_DIVIDE }

(* relational operators *)
| "==" { IS_EQUAL }
| ">=" { IS_GEQ }
| '>' { IS_GT }
| "<=" { IS_LEQ }
| '<' { IS_LT }
| "!=" { IS_NOT_EQUAL }

(* logical operators *)
| "&&" { AND }
| "||" { OR }
| '!' { NOT }

(* parentheses and brackets *)
| '(' { LEFT_PARENTHESIS }
| ')' { RIGHT_PARENTHESIS }
| '{' { LEFT_CURLY_BRACKET }
| '}' { RIGHT_CURLY_BRACKET }
| '[' { LEFT_SQUARE_BRACKET }
| ']' { RIGHT_SQUARE_BRACKET }

(* delimiters *)
| ',' { COMMA }
| ':' { COLON }

(* assignment *)
| "=" { ASSIGNMENT }

(* keywords *)
| "if" { IF }
| "elif" { ELIF }
| "else" { ELSE }
| "for" { FOR }
| "while" { WHILE }
| "in" { IN }
(* | "continue" { CONTINUE }
| "break" { BREAK } *)
| "return" { RETURN }
(* | "exit" { EXIT } *)
| "def" { DEFINE }
| "int" { INT }
| "float" { FLOAT }
| "var" { VAR }
| "nil" { NIL }

(* build-in function*)
| "any" { ANY }
| "all" { ALL }
| "sum" { SUM }
| "ones" { ONES }
| "zeros" { ZEROS }
| "rand" { RAND }
| "int_of" { INT_OF }
| "float_of" { FLOAT_OF }
| "floor" { FLOOR }
| "ceil" { CEIL }
| "round" { ROUND }
| "abs" { ABS }
| "log" { LOG } 
| "inverse" { INVERSE }
(* | "solve" { SOLVE }
| "svd" { SVD }
| "eig" { EIG }
| "eign" { EIGV } *)
| "print" { PRINT }
| "print_error" { PRINT_ERROR }
| "shape" { SHAPE }
| "cat" { CAT }
(* Parallel Environment keywords *)
| "parallel_define" { PARALLEL_DEFINE }
| "overload" { OVERLOAD }
| "map" { MAP }
| "reduce" { REDUCE }
| "using" { USING }
| "end" { END }

(* Operator Names *)
| "__"['*''+''-']"__" as oname { OPERATOR_INDICATOR(oname) }

(* identifiers and literals *)
| digits as lit { INT_LITERAL(int_of_string lit) }
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as id { IDENTIFIER(id) }
| ('"'[^'"''\\']*('\\'_[^'"''\\']*)*'"') as str { STRING_LITERAL(String.sub str 1 (String.length str - 2)) }
| (digits '.' digit* exponent? | digits exponent | '.' digits exponent?) as lit { FLOAT_LITERAL(float_of_string lit) }

| eof { EOF }

(* handle illegal character*)
| _ as ch {raise (Failure("illegal character " ^ Char.escaped ch))}


and comment_line = parse
 '\n' { tokenize lexbuf }
| _ { comment_line lexbuf }

and comment_block = parse
 "'''" { tokenize lexbuf }
| _ { comment_block lexbuf }
