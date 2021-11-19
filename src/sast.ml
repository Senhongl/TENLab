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
  SLit(l) -> string_of_lit l
| SId(str1) -> str1
| SBinop(se1, bop, se2) -> string_of_sexpr se1 ^ " " ^ string_of_bop bop ^ " " ^ string_of_sexpr se2
| SUnop(uop, se1) -> string_of_uop uop ^ " " ^ string_of_sexpr se1
| SRange(se1, se2, se3) -> string_of_sexpr se1 ^ ":" ^ string_of_sexpr se2 ^ ":" ^ string_of_sexpr se3
| SReturn(se1) -> "return " ^ string_of_sexpr se1 ^ "\n"
| SBreak -> "break\n"
| SContinue -> "continue\n"
| SExit(se1) -> "exit" ^ string_of_sexpr se1 ^ "\n"

(* built-in functions *)
| SPrint(se1) -> "print(" ^ string_of_sexpr se1 ^ ")"
(* | Shape(e1) -> "shape(" ^ string_of_expr e1 ^ ")"
| Cat(e1, e2, e3) -> "cat(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ string_of_expr e3 ^ ")"
| Any(e1) -> "any (" ^ string_of_expr e1 ^ ")"
| All(e1) -> "all (" ^ string_of_expr e1 ^ ")"
| Sum(e1) -> "sum (" ^ string_of_expr e1 ^ ")"
| Ones(e1) -> "ones (" ^ string_of_expr e1 ^ ")"
| Zeros(e1) -> "zeros (" ^ string_of_expr e1 ^ ")"
| Len(e1) -> "len (" ^ string_of_expr e1 ^ ")"
| Int_Of(e1) -> "int_of (" ^ string_of_expr e1 ^ ")"
| Float_Of(e1) -> "float_of (" ^ string_of_expr e1 ^ ")"
| Floor(e1) -> "floor (" ^ string_of_expr e1 ^ ")"
| Ceil(e1) -> "ceil (" ^ string_of_expr e1 ^ ")"
| Round(e1) -> "round (" ^ string_of_expr e1 ^ ")"
| Abs(e1) -> "abs(" ^ string_of_expr e1 ^ ")"
| Log(e1) -> "log (" ^ string_of_expr e1 ^ ")"
| Inverse(e1) -> "inverse (" ^ string_of_expr e1 ^ ")"
| Solve(e1, e2) -> "solve (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
| Svd(e1) -> "svd (" ^ string_of_expr e1 ^ ")"
| Eig(e1) -> "eig (" ^ string_of_expr e1 ^ ")"
| Eigv(e1) -> "Eigv (" ^ string_of_expr e1 ^ ")"
| FuncCall(e1, e2) -> (string_of_expr e1 ^ "(" ^ (String.concat "," (List.map string_of_expr e2))) ^ ")" *)

| STensor0(se1) -> string_of_sexpr se1
| SLRTensor(se1) -> "[" ^ string_of_sexpr se1 ^ "]"
| SNPTensor(se1) -> string_of_sexpr se1
| SLRTensors(se1, se2) -> "[" ^ string_of_sexpr se1 ^ ", " ^ string_of_sexpr se2 ^ "]"
| SNPTensors(se1, se2) -> string_of_sexpr se1 ^ ", " ^ string_of_sexpr se2
| SIntTensor(se1) -> "int(" ^ string_of_sexpr se1 ^ ")"
| SFloatTensor(se1) -> "float(" ^ string_of_sexpr se1 ^ ")"
| SVarTensor(se1) -> "var(" ^ string_of_sexpr se1 ^ ")"

let rec string_of_sstmt = function
  SExpr(se1) -> string_of_sexpr se1 ^ ";"

and string_of_sprogram l = String.concat "" (List.map string_of_sstmt l) ^ "\n"