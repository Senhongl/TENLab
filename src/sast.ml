(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

(* a hello world version of sast *)
open Ast

type sexpr = 
  SLit of literal
| SPrint of sexpr

type sstmt =
  SExpr of sexpr

let rec string_of_sexpr = function
  SLit(l) -> Ast.string_of_lit l
| SPrint(se1) -> "print(" ^ string_of_sexpr se1 ^ ");"

let rec string_of_sstmt = function
  SExpr(se1) -> string_of_sexpr se1

and string_of_sprogram l = String.concat "" (List.map string_of_sstmt l) ^ "\n"