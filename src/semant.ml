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
let function_table = StringHash.create 10

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
let rec check_expr symbol_table function_table = function
  Id(id) -> if StringHash.mem symbol_table id then StringHash.find symbol_table id
            else raise (E "not defined")
| FId(id) -> if StringHash.mem function_table id then (SVoidTup, SFId(id)) else raise (E "function not defined")
| Binop(x1, bop, x2) -> (SVoidTup, SBinop(check_expr symbol_table function_table x1, bop, check_expr symbol_table function_table x2))
| Tensor(x) -> (match check_tensor(x) with 
  | (TensorTup(t, n, d::d_), y) -> (STensorTup(t, n, Array.of_list d_), STensor(y))
  | (_, _) -> raise (E "ought not occur"))
| Print(e) -> (SVoidTup, SPrint(check_expr symbol_table function_table e))
| FuncCall(e1, e2) -> let e1_ = check_expr symbol_table function_table e1 in
                      let e2_ = List.map (check_expr symbol_table function_table) e2 in
                      let FId(id) = e1 in
                      let argc = StringHash.find function_table id in
                      if argc <> List.length(e2) then raise (E "the number of arguments mismatch")
                      else (SVoidTup, SFuncCall(e1_, e2_))

(* stmt -> sstmt *)
let check_stmt symbol_table function_table = function
  Expr(e) -> SExpr(check_expr symbol_table function_table e)
| Assign(str1, e2) -> let sexpr = check_expr symbol_table function_table e2 in
                      ignore(StringHash.add symbol_table str1 sexpr); SAssign(str1, sexpr)
| FuncSign(str1, str2) -> let argc = List.length(str2) in
                          StringHash.add function_table str1 argc; SFuncSign(str1, str2)

let check stmts = 
  List.map (check_stmt symbol_table function_table) stmts