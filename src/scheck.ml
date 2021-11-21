open Ast
open Echeck

exception E of string

module StringMap = Map.Make(String)

let stmt_check symbol_table = function
  Expr(e) -> expr_check symbol_table e
(* | Assign(e1, e1) -> let tup = expr_check symbol_table e1 in
                      ignore(StringMap.add str1 tup symbol_table); tup *)