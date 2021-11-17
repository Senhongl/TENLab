(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

(* a hello world version of sast *)
open Ast

type sexpr =
  SLit of literal
| SElements of sexpr list
| SIntTensor of sexpr
| SFloatTensor of sexpr
| SVarTensor of sexpr
| SConcatTensor of sexpr * sexpr
| SOpenTensor of sexpr
| SCloseTensor of sexpr
| SBinop of sexpr * bop * sexpr
| SUnop of uop * sexpr
| SRange of sexpr * sexpr * sexpr
(* Keyword expression *)
| SReturn of sexpr
| SBreak
| SContinue
| SExit
(* Built-in functions *)
| SPrint of sexpr
| SShape of sexpr
| SCat of sexpr * sexpr
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

| SFuncCall of string * sexpr list
| SAssign of string * sexpr

type sstmt =
  SExpr of sexpr

type sprogram = (string * pofunc list) list * sstmt list

let rec string_of_sexpr = function
  SLit(l) -> Ast.string_of_lit l
| SPrint(se1) -> "print(" ^ string_of_sexpr se1 ^ ");"

let rec string_of_sstmt = function
  SExpr(se1) -> string_of_sexpr se1


let string_of_sprogram (pes,stmts) =
    String.concat "" (List.map Ast.string_of_pe pes) ^
    String.concat "" (List.map string_of_sstmt stmts) ^ "\n"
