(* Semantic checking for the TENLab compiler *)

open Ast
open Sast

exception E of string

module StringHash = Hashtbl.Make(struct
  type t = string
  let equal x y = x = y
  let hash = Hashtbl.hash
end)

let symbol_table = StringHash.create 10

let name_error id = "name " ^ id ^ " is not defined" 
and invalid_type op = "invalid type for " ^ op
and invalid_dim err = "invalid dimension: " ^ err
and dummy_error = "dummy error"
and make_err er = raise (E er)

let rec equal_dim d1 d2=
  match d1, d2 with
    d10::d1_, d20::d2_ -> if d10 <> d20 then false else equal_dim d1_ d2_
  | [], [] -> true
  | _, [] -> false
  | [], _ -> false

let rec check_tensor = function
  Tensor0(x) -> (match x with
    IntLit(y) -> (TensorTup(INT_Tensor, 0, [-1]), [|x|])
  | FloatLit(y) -> (TensorTup(FLOAT_Tensor, 0, [-1]), [|x|]))
| LRTensor(x) -> 
    (match check_tensor(x) with
      (TensorTup(t, nd, d0::d_), y) -> (TensorTup(t, nd+1, -1::-d0::d_), y)
    | _ -> raise (E "ought not occur"))
| NPTensor(x) -> check_tensor(x)
| LRTensors(x1, x2) -> 
    let tdy1 = check_tensor(x1) in
    let tdy2 = check_tensor(x2) in
      (match tdy1, tdy2 with
        (TensorTup(t1, nd1, d10::d1_), y1), (TensorTup(t2, nd2, d20::d2_), y2) -> 
          if t1 = t2 && equal_dim d1_ d2_ then 
            (TensorTup(t1, nd1+1, -1::-(d10 + d20)::d1_), Array.append y1 y2)
          else if t1 <> t2 then raise (E "invalid type")
          else raise (E "invalid dim")
      | _, _ -> raise (E "ought not occur"))
| NPTensors(x1, x2) -> 
    let tdy1 = check_tensor(x1) in
    let tdy2 = check_tensor(x2) in
      (match tdy1, tdy2 with
        (TensorTup(t1, nd1, d10::d1_), y1), (TensorTup(t2, nd2, d20::d2_), y2) -> 
          if t1 = t2 && equal_dim d1_ d2_ then 
            (TensorTup(t1, nd1, (d10 + d20)::d1_), Array.append y1 y2)
          else if t1 <> t2 then raise (E "invalid type")
          else raise (E "invalid dim")
      | _, _ -> raise (E "ought not occur"))

(* expr -> sexpr *)
let rec check_expr symbol_table = function
  Id(id) -> if StringHash.mem symbol_table id then StringHash.find symbol_table id
            else raise (E "not defined")
| Binop(x1, bop, x2) -> (SVoidTup, SBinop(check_expr symbol_table x1, bop, check_expr symbol_table x2))
| Tensor(x) -> (match check_tensor(x) with 
  | (TensorTup(t, n, d::d_), y) -> (STensorTup(t, n, Array.of_list d_), STensor(y))
  | (_, _) -> raise (E "ought not occur"))
| Print(e) -> (SVoidTup, SPrint(check_expr symbol_table e))

(* stmt -> sstmt *)
let check_stmt symbol_table = function
  Expr(e) -> SExpr(check_expr symbol_table e)
| Assign(str1, e2) -> let sexpr = check_expr symbol_table e2 in
                      ignore(StringHash.add symbol_table str1 sexpr); SAssign(str1, sexpr)

let check stmts = 
  (* let rec expr = function
    Id(i) -> SId(i)
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

  in *)

  (* let rec stmt = function
    Expr(e) -> SExpr(expr e)
  | Assign(e1, e2) -> SAssign(expr e1, expr e2)

  in *)

  List.map (check_stmt symbol_table) stmts
  (* List.map stmt stmts *)