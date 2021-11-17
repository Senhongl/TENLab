(* Semantic checking for the TENLab compiler *)

open Ast
open Sast

(* a hello world version of semantic check *)
(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)



let check (pe,stmts) =
  let rec expr = function
    Lit(l) -> SLit(l)
  | Print(e) -> SPrint(expr e) in
  (pe, List.map (function stmt -> match stmt with
              Expr(e) -> SExpr(expr e)) stmts)
