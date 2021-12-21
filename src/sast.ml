(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

(* a hello world version of sast *)
open Ast

type sdimtype = STensorTup of tensortype * int * int array | SVoidTup

(*type index = int * int array * literal array*)

type sexpr = sdimtype * sx
and sx =
  SVoidExpr (* for semantic check only *)
| SFId of string
| STensor of literal array
| SVtensor of sexpr list
| SASexpr of sasexpr
| SStringLit of string
| SBinop of sexpr * bop * sexpr
| SUnop of uop * sexpr
| SRange of sexpr * sexpr * sexpr
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
| SFuncCall of string * sexpr list

and sasexpr = 
  Identifier of string
| IdentifierInd of string * sexpr list

type sstmt =
  SEmptyStmt
| SExpr of sexpr
| SAssign of sasexpr * sexpr
(* | SFuncSign of string * string list *)
| SFuncDecl of string * string list * sstmt list
| SIfStmt of sexpr * sstmt list * sstmt list
| SWhileStmt of sexpr * sstmt list
| SForStmt of string * sexpr * sstmt list
(* Keyword statement *)
| SPEInvoke of string
| SPEEnd of string
| SReturn of sexpr
(* | SBreak
| SContinue
| SExit of sexpr *)

type spofunc = {
  soperator : string;
  sparams : string list;
  smapfuncs : (string * sstmt list) list;
  sreducefunc : sstmt list;
}

type accspofunc = SDEF | SPO of spofunc

type spe = {
  sadd : accspofunc;
  sminus : accspofunc;
  smulti : accspofunc;
}

type program = (string * spe) list * sstmt list

(* let rec string_of_sexpr = function
  SId(str1) -> str1
| STensor(st1) -> string
| SBinop(se1, bop, se2) -> string_of_sexpr se1 ^ " " ^ string_of_bop bop ^ " " ^ string_of_sexpr se2
| SUnop(uop, se1) -> string_of_uop uop ^ " " ^ string_of_sexpr se1
| SRange(se1, se2, se3) -> string_of_sexpr se1 ^ ":" ^ string_of_sexpr se2 ^ ":" ^ string_of_sexpr se3

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

let rec string_of_sstmt = function
  SExpr(se1) -> string_of_sexpr se1 ^ ";"
| SReturn(se1) -> "return " ^ string_of_sexpr se1 ^ "\n"
| SBreak -> "break\n"
| SContinue -> "continue\n"
| SExit(se1) -> "exit" ^ string_of_sexpr se1 ^ "\n"

and string_of_sprogram l = String.concat "" (List.map string_of_sstmt l) ^ "\n" *)
let string_of_sprogram _ = "TODO!"