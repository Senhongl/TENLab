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

open Parsing;;
let _ = parse_error;;
# 1 "tensorparser.mly"
 open Tensorast 
# 18 "tensorparser.ml"
let yytransl_const = [|
  257 (* L *);
  258 (* C *);
  259 (* R *);
  260 (* LP *);
  261 (* RP *);
    0 (* EOF *);
  262 (* INT *);
  263 (* FLOAT *);
    0|]

let yytransl_block = [|
  264 (* INT_LITERAL *);
  265 (* FLOAT_LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\002\000\002\000\002\000\003\000\
\003\000\000\000"

let yylen = "\002\000\
\001\000\001\000\004\000\004\000\005\000\003\000\001\000\003\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\001\000\002\000\010\000\000\000\
\000\000\000\000\007\000\000\000\000\000\000\000\003\000\004\000\
\000\000\006\000\000\000\000\000\000\000\005\000\008\000"

let yydgoto = "\002\000\
\011\000\019\000\020\000"

let yysindex = "\013\000\
\251\254\000\000\011\255\012\255\000\000\000\000\000\000\255\254\
\255\254\255\254\000\000\013\255\014\255\010\255\000\000\000\000\
\255\254\000\000\015\255\017\255\255\254\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\018\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\021\000\001\000\002\000"

let yytablesize = 23
let yytable = "\010\000\
\003\000\004\000\005\000\006\000\003\000\004\000\005\000\006\000\
\012\000\013\000\014\000\017\000\018\000\001\000\008\000\009\000\
\021\000\015\000\016\000\022\000\009\000\007\000\023\000"

let yycheck = "\001\001\
\006\001\007\001\008\001\009\001\006\001\007\001\008\001\009\001\
\008\000\009\000\010\000\002\001\003\001\001\000\004\001\004\001\
\002\001\005\001\005\001\003\001\003\001\001\000\021\000"

let yynames_const = "\
  L\000\
  C\000\
  R\000\
  LP\000\
  RP\000\
  EOF\000\
  INT\000\
  FLOAT\000\
  "

let yynames_block = "\
  INT_LITERAL\000\
  FLOAT_LITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 14 "tensorparser.mly"
              ( Lit(IntLit(_1)) )
# 98 "tensorparser.ml"
               : Tensorast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 15 "tensorparser.mly"
                ( Lit(FloatLit(_1)) )
# 105 "tensorparser.ml"
               : Tensorast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Tensorast.expr) in
    Obj.repr(
# 16 "tensorparser.mly"
                   ( IntTensor(_3) )
# 112 "tensorparser.ml"
               : Tensorast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Tensorast.expr) in
    Obj.repr(
# 17 "tensorparser.mly"
                     ( FloatTensor(_3) )
# 119 "tensorparser.ml"
               : Tensorast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Tensorast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Tensorast.expr) in
    Obj.repr(
# 20 "tensorparser.mly"
                          ( LRTensors(_2, _4) )
# 127 "tensorparser.ml"
               : Tensorast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tensorast.expr) in
    Obj.repr(
# 21 "tensorparser.mly"
               ( LRTensor(_2) )
# 134 "tensorparser.ml"
               : Tensorast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tensorast.expr) in
    Obj.repr(
# 22 "tensorparser.mly"
         ( Tensor0(_1) )
# 141 "tensorparser.ml"
               : Tensorast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tensorast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tensorast.expr) in
    Obj.repr(
# 25 "tensorparser.mly"
                      ( NPTensors(_1, _3) )
# 149 "tensorparser.ml"
               : Tensorast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tensorast.expr) in
    Obj.repr(
# 26 "tensorparser.mly"
           ( NPTensor(_1) )
# 156 "tensorparser.ml"
               : Tensorast.expr))
(* Entry expr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Tensorast.expr)
