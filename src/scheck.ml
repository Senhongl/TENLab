open Ast
open Sast
open Echeck

exception E of string

module StringMap = Map.Make(String)

let check_stmt symbol_table stmt = match stmt with
  Expr(e) -> SExpr(check_expr symbol_table e)
(* | Assign(e1, e1) -> let tup = expr_check symbol_table e1 in
                      ignore(StringMap.add str1 tup symbol_table); tup *)