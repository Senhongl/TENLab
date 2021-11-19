(* Semantic checking for the TENLab compiler *)

open Ast
open Sast
open Echeck

module StringMap = Map.Make(String)
(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)
let symbol_table = StringMap.empty

let check stmts = 
  let rec expr = function
    Lit(l) -> SLit(l)
  | Id(i) -> SId(i)
  | IntTensor(e1) -> SIntTensor(expr e1)
  | FloatTensor(e1) -> SFloatTensor(expr e1)
  | VarTensor(e1) -> SVarTensor(expr e1)
  | LRTensor(e1) -> SLRTensor(expr e1)
  | NPTensor(e1) -> SNPTensor(expr e1)
  | LRTensors(e1, e2) -> SLRTensors(expr e1, expr e2)
  | NPTensors(e1, e2) -> SNPTensors(expr e1, expr e2)
  | Tensor0(e1) -> STensor0(expr e1)
  | Binop(e1, bop, e2) -> SBinop(expr e1, bop, expr e2)
  | Unop(uop, e1) -> SUnop(uop, expr e1)
  | Range(e1, e2, e3) -> SRange(expr e1, expr e2, expr e3)
  (* Keyword expression *)
  | Return(e1) -> SReturn(expr e1)
  | Break -> SBreak
  | Continue -> SContinue
  | Exit(e1) -> SExit(expr e1)
  (* Built-in functions *)
  | Print(e1) -> SPrint(expr e1)
  | Shape(e1) -> SShape(expr e1)
  | Cat(e1, e2, e3) -> SCat(expr e1, expr e2, expr e3)
  | Any(e1) -> SAny(expr e1)
  | All(e1) -> SAll(expr e1)
  | Sum(e1) -> SSum(expr e1)
  | Ones(e1) -> SOnes(expr e1)
  | Zeros(e1) -> SZeros(expr e1)
  | Len(e1) -> SLen(expr e1)
  | Int_Of(e1) -> SInt_Of(expr e1)
  | Float_Of(e1) -> SFloat_Of(expr e1)
  | Floor(e1) -> SFloor(expr e1)
  | Ceil(e1) -> SCeil(expr e1)
  | Round(e1) -> SRound(expr e1)
  | Abs(e1) -> SAbs(expr e1)
  | Log(e1) -> SLog(expr e1)
  | Inverse(e1) -> SInverse(expr e1)
  | Solve(e1, e2) -> SSolve(expr e1, expr e2)
  | Svd(e1) -> SSvd(expr e1)
  | Eig(e1) -> SEig(expr e1)
  | Eigv(e1) -> SEigv(expr e1)
  | FuncCall(e1, e2) -> SFuncCall(expr e1, List.map expr e2)

  in

  ignore(List.map (function stmt -> match stmt with
                    Expr(e) -> expr_check symbol_table e) stmts);
  List.map (function stmt -> match stmt with
              Expr(e) -> SExpr(expr e)) stmts