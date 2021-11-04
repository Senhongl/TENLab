{
        open Parser
}

rule tokenize = parse 
  [' ' '\t'] { tokenize lexbuf }
| ['\r' '\n'] { NEWLINE }

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
| "continue" { CONTINUE }
| "break" { BREAK }
| "return" { RETURN }
(* TODO: | "read" { READ }
| "print" { PRINT } *)
| "exit" { EXIT }
| "def" { DEFINE }
| "int" { INT }
| "float" { FLOAT }
(* TODO: string or char? do we need to support these two keywords? *)
(* TODO: | "var" { VOID_TENSOR } *)
(* TODO: | "cat" { CAT }
| "shape" { SHAPE } *)

(* identifiers and literals *)
(* TODO: work for negative sign? *)
| ['0'-'9']+ as lit { INT_LITERAL(int_of_string lit) }
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as id { IDENTIFIER(id) }
| ('"'[^'"''\\']*('\\'_[^'"''\\']*)*'"') as str { STRING_LITERAL(String.sub str 1 (String.length str - 2)) }
| (((['0'-'9']*)'.'(['0'-'9']+)('e'['-''+']?['0'-'9']+)?) | 
   ((['0'-'9']+)('e'['-''+']?['0'-'9']+)?)) as flt { FLOAT_LITERAL(float_of_string flt) }

| eof { EOF }


and comment_line = parse 
 '\n' { tokenize lexbuf }
| _ { comment_line lexbuf }

and comment_block = parse 
 "'''" { tokenize lexbuf }
| _ { comment_block lexbuf }