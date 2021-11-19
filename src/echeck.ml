open Ast

exception E of string

module StringMap = Map.Make(String)

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

type tensortype = FLOAT_Tensor | INT_Tensor | STRING_Tensor | VAR_Tensor

type dimtype = CheckTup of tensortype * int list

let rec expr_check symbol_table expr = match expr with
  Lit(l) -> (match l with
    IntLit(_) -> CheckTup(INT_Tensor, [-1])
  | FloatLit(_) -> CheckTup(FLOAT_Tensor, [-1])
  | StringLit(_) -> CheckTup(STRING_Tensor, [-1]))
| Id(id) -> if StringMap.mem id symbol_table then StringMap.find id symbol_table
            else make_err (name_error id)
| IntTensor(x) -> (match expr_check symbol_table x with
    CheckTup(t, d) -> 
        if t = INT_Tensor then CheckTup(t, d) else make_err (invalid_type "tensor declaration"))
| FloatTensor(x) -> (match expr_check symbol_table x with
    CheckTup(t, d) -> 
        if t = FLOAT_Tensor then CheckTup(t, d) else make_err (invalid_type "tensor declaration"))
| Tensor0(x) -> expr_check symbol_table x
| LRTensors(x1, x2) -> 
    let td1 = expr_check symbol_table x1 in
    let td2 = expr_check symbol_table x2 in
      (match td1, td2 with
        CheckTup(t1, d10::d1_), CheckTup(t2, d20::d2_) -> 
          if t1 = t2 && equal_dim d1_ d2_ then CheckTup(t1, -1::-(d10 + d20)::d1_)
          else if t1 <> t2 then make_err (invalid_type "tensor declaration")
          else make_err (invalid_dim "tensor declaration")
      | _, _ -> make_err dummy_error)
| LRTensor(x) -> 
    (match expr_check symbol_table x with
      CheckTup(t, d0::d_) -> CheckTup(t, -1::-d0::d_)
    | CheckTup(_, []) -> make_err dummy_error)
| NPTensor(x) -> expr_check symbol_table x
| NPTensors(x1, x2) -> 
    let td1 = expr_check symbol_table x1 in
    let td2 = expr_check symbol_table x2 in
      (match td1, td2 with
        CheckTup(t1, d10::d1_), CheckTup(t2, d20::d2_) -> 
          if t1 = t2 && equal_dim d1_ d2_ then CheckTup(t1, (d10 + d20)::d1_)
          else if t1 <> t2 then make_err (invalid_type "tensor declaration")
          else make_err (invalid_dim "tensor declaration")
      | _, _ -> make_err dummy_error)
| Binop(e1, bop, e2) -> 
    let CheckTup(t1, d10::d1_) = expr_check symbol_table e1 in
    let CheckTup(t2, d20::d2_) = expr_check symbol_table e2 in
    let t3 = 
      (match t1, t2 with 
        STRING_Tensor, _ -> make_err (invalid_type "binary operator")
      | _, STRING_Tensor -> make_err (invalid_type "binary operator")
      | VAR_Tensor, _ -> make_err (invalid_type "binary operator")
      | _, VAR_Tensor -> make_err (invalid_type "binary operator")
      | INT_Tensor, INT_Tensor -> INT_Tensor
      | _, _ -> FLOAT_Tensor) in

      (match bop with
        Add | Sub -> if equal_dim d1_ d2_ then CheckTup(t3, d10::d1_)
               else if List.length(d2_) = 0 then CheckTup(t3, d10::d1_)
               else make_err (invalid_dim "operands don't support with different shapes")
      | Mul -> if List.length(d1_) <> 2 && List.length(d2_) <> 2 then make_err (invalid_dim "operands support only 2-dim multiplication") 
               else if List.hd(List.tl(d1_)) <> List.hd(d2_) then make_err (invalid_dim "mismatch of the dimension between two tensor")
               else if List.hd(d1_) = 2 && List.length(d2_) = 2 && List.hd(List.tl(d1_)) = List.hd(d2_) then CheckTup(t3, -1::[List.hd(d1_); List.hd(List.tl(d2_))])
               else make_err dummy_error
      | DotMul | DotPow -> if equal_dim d1_ d2_ then CheckTup(t3, d10::d1_)
                           else make_err (invalid_dim "operands don't support with different shapes")
      | Div -> if List.length(d2_) = 0 then CheckTup(FLOAT_Tensor, d10::d1_)
               else make_err (invalid_dim "second tensor should be 0-dim")
      | Pow | Mod -> if List.length(d2_) = 0 then CheckTup(t3, d10::d1_)
                     else make_err (invalid_dim "second tensor should be 0-dim")
      | FlrDiv -> if List.length(d2_) = 0 then CheckTup(INT_Tensor, d10::d1_)
                  else make_err (invalid_dim "second tensor should be 0-dim")
      )
| Unop(uop, e1) -> 
    let CheckTup(t1, d10::d1_) = expr_check symbol_table e1 in

    (match uop with
      Transpose -> CheckTup(t1, d10::List.rev d1_)
    | Not | Neg -> 
      (match t1 with
        INT_Tensor | FLOAT_Tensor -> CheckTup(t1, d10::d1_)
      | FLOAT_Tensor | VAR_Tensor -> make_err (invalid_type "unary operator")))
| Print(e1) -> expr_check symbol_table e1
(* | Range(e1, e2, e3) -> 
    let CheckTup(t1, d10::d1_) = check_expr(e1) in
    let CheckTup(t2, d20::d2_) = check_expr(e2) in
    let CheckTup(t3, d30::d3_) = check_expr(e3) in
    let t4 =
      (match t1, t2, t3 with
        INT_Tensor, INT_Tensor, INT_Tensor -> INT_Tensor
      | _, _, _ -> make_err (invalid_type "creating range")) in
    
    if List.length(d1_) = 0 && List.length(d2_) = 0 && List.length(d3_) = 0 then CheckTup(t1, d10::d1_)
| FuncCall(e1, e2) -> 
    let  *)







                      