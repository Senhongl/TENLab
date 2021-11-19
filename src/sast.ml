(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

(* a hello world version of sast *)
open Ast

type sexpr = 
  SLit of literal
| SId of string
| SIntTensor of sexpr
| SFloatTensor of sexpr
| SVarTensor of sexpr
| SLRTensor of sexpr
| SNPTensor of sexpr
| SLRTensors of sexpr * sexpr
| SNPTensors of sexpr * sexpr
| STensor0 of sexpr
| SBinop of sexpr * bop * sexpr
| SUnop of uop * sexpr
| SRange of sexpr * sexpr * sexpr
(* Keyword expression *)
| SReturn of sexpr
| SBreak
| SContinue
| SExit of sexpr
(* Built-in functions *)
| SPrint of sexpr
| SShape of sexpr
| SCat of sexpr * sexpr * sexpr
| SAny of sexpr
| SAll of sexpr
| SSum of sexpr
| SOnes of sexpr
| SZeros of sexpr
| SLen of sexpr
| SInt_Of of sexpr
| SFloat_Of of sexpr
| SFloor of sexpr
| SCeil of sexpr
| SRound of sexpr
| SAbs of sexpr
| SLog of sexpr
| SInverse of sexpr
| SSolve of sexpr * sexpr
| SSvd of sexpr
| SEig of sexpr
| SEigv of sexpr
| SFuncCall of sexpr * sexpr list

type sstmt =
  SExpr of sexpr

let rec string_of_sexpr = function
  SLit(l) -> Ast.string_of_lit l
| SPrint(se1) -> "print(" ^ string_of_sexpr se1 ^ ")"

let rec string_of_sstmt = function
  SExpr(se1) -> string_of_sexpr se1 ^ ";"

and string_of_sprogram l = String.concat "" (List.map string_of_sstmt l) ^ "\n"