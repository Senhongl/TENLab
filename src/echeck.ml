open Ast

exception E of string

let make_err er = raise (E er) in

let name_error id = "name " ^ id ^ " is not defined" in
let invalid_type op = "invalid type for " ^ op in
let invalid_dim err = "invalid dimension: " ^ err in
let dummy_error = "dummy error" in

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
    IntLit(l) -> CheckTup(INT_Tensor, [-1])
  | FloatLit(l) -> CheckTup(FLOAT_Tensor, [-1]))
  
| Id(id) -> if StringMap.mem id symbol_table then StringMap.mem find id symbol_table
            else make_err (name_error id)
| IntTensor(x) -> (match check_expr(x) with
    CheckTup(t, d) -> 
        if t = INT_Tensor then CheckTup(t, d) else make_err (invalid_type "tensor declaration"))
| FloatTensor(x) -> (match check_expr(x) with
    CheckTup(t, d) -> 
        if t = FLOAT_Tensor then CheckTup(t, d) else make_err (invalid_type "tensor declaration"))
| Tensor0(x) -> check_expr(x)
| LRTensors(x1, x2) -> 
    let td1 = check_expr(x1) in
    let td2 = check_expr(x2) in
      (match td1, td2 with
        CheckTup(t1, d10::d1_), CheckTup(t2, d20::d2_) -> 
          if t1 = t2 && equal_dim d1_ d2_ then CheckTup(t1, -1::-(d10 + d20)::d1_)
          else if t1 <> t2 then make_err ivalid_type
          else make_err (invalid_dim "tensor declaration")
      | _, _ -> make_err dummy_error)
| LRTensor(x) -> 
    (match check_expr(x) with
      CheckTup(t, d0::d_) -> CheckTup(t, -1::-d0::d_)
    | CheckTup(_, []) -> make_err dummy_error)
| NPTensor(x) -> check_expr(x)
| NPTensors(x1, x2) -> 
    let td1 = check_expr(x1) in
    let td2 = check_expr(x2) in
      (match td1, td2 with
        CheckTup(t1, d10::d1_), CheckTup(t2, d20::d2_) -> 
          if t1 = t2 && equal_dim d1_ d2_ then CheckTup(t1, (d10 + d20)::d1_)
          else if t1 <> t2 then make_err (invalid_type "tensor declaration")
          else make_err (invalid_dim "tensor declaration")
      | _, _ -> make_err dummy_error
| Binop(e1, bop, e2) -> 
    let CheckTup(t1, d10::d1_) = check_expr(e1) in
    let CheckTup(t2, d20::d2_) = check_expr(e2) in
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
               else if List.tl(d1_) <> List.hd(d2_) then make_err (invalid_dim "mismatch of the dimension between two tensor")
               else if List.hd(d1_) = 2 && List.length(d2_) = 2 && List.tl(d1_) = List.hd(d2_) then CheckTup(t3, -1::[List.hd(d1_); List.tl(d2_)])
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
    let CheckTup(t1, d10::d1_) = check_expr(e1) in

    (match uop with
      Transpose -> CheckTup(t1, d10::List.rev d1_)
    | Not | Neg -> 
      (match t1 with
        INT_Tensor | FLOAT_Tensor -> CheckTup(t1, d10::d1_)
      | FLOAT_Tensor | VAR_Tensor -> make_err (invalid_type "unary operator")))

(* | Range(e1, e2, e3) -> 
    let CheckTup(t1, d10::d1_) = check_expr(e1) in
    let CheckTup(t2, d20::d2_) = check_expr(e2) in
    let CheckTup(t3, d30::d3_) = check_expr(e3) in
    CheckTup
    let t4 =
      (match t1, t2, t3 with
        INT_Tensor, INT_Tensor, INT_Tensor -> INT_Tensor
      | _, _, _ -> make_err (invalid_type "creating range")) in
    
    if List.length(d1_) = 0 && List.length(d2_) = 0 && List.length(d3_) = 0 then CheckTup(t1, d10::d1_) *)
(* | FuncCall(e1, e2) -> 
    let  *)

(* check_build-in_functions *)
(* only IntTensor allowed in Any(), as we treat True and False as 1 and 0, return a 0-dim IntTensor() *)
| Any(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | INT_Tensor -> CheckTup(IntTensor, d10::[]) 
      | STRING_Tensor | FloatTensor | VAR_Tensor -> make_err (invalid_type "Any() function, only IntTensor allowed") 
    ) 
(* only IntTensor allowed in All(), as we treat True and False as 1 and 0, return a 0-dim IntTensor() *)
| All(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | INT_Tensor -> CheckTup(IntTensor, d10::[]) 
      | STRING_Tensor | FloatTensor | VAR_Tensor -> make_err (invalid_type "All() function, only IntTensor allowed") 
    ) 
(* only IntTensor and FloatTensor allowed in Sum(), return a 0-dim tensor with corresponding type tensor *)
| Sum(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | INT_Tensor | FloatTensor  -> CheckTup(t1, d10::[]) 
      | STRING_Tensor | VAR_Tensor -> make_err (invalid_type "Sum() function, only IntTensor and FloatTensor allowed") 
    ) 
(* only 0-dim or 1-dim IntTensor allowed in Ones(), e.g. Ones(1) or Ones(IntTensor([1,2,3])), return FloatTensor() *)
| Ones(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | FloatTensor -> make_err (invalid_type "Ones() function, only IntTensor allowed")
      | STRING_Tensor -> make_err (invalid_type "Ones() function, only IntTensor allowed") 
      | VAR_Tensor -> make_err (invalid_type "Ones() function, only IntTensor allowed")
    ) in
    if List.length(d1_) <= 1 then CheckTup(FLOAT_Tensor, d10::d1_) (* TODO: need to know exact dims to create tensor *)
    else make_err (invalid_dim "tensor should be 0-dim or 1-dim") 
(* only 0-dim or 1-dim IntTensor allowed in Zeros(), e.g. Zeros(1) or Zeros(IntTensor([1,2,3])), return FloatTensor() *) 
| Zeros(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | FloatTensor -> make_err (invalid_type "Zeros() function, only IntTensor allowed")
      | STRING_Tensor -> make_err (invalid_type "Zeros() function, only IntTensor allowed") 
      | VAR_Tensor -> make_err (invalid_type "Zeros() function, only IntTensor allowed")
    ) in
    if List.length(d1_) <= 1 then CheckTup(FLOAT_Tensor, d10::d1_) (* TODO: need to know exact dims to create tensor *)
    else make_err (invalid_dim "tensor should be 0-dim or 1-dim")  
(* return the first dimension as a 0-dim IntTensor *)
| Len(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in CheckTup(IntTensor, d10::[]) 
(* return IntTensor with same shape, string literal not allowed *)
| Int_Of(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | STRING_Tensor | VAR_Tensor -> make_err (invalid_type "INT_OF() function") 
    ) in CheckTup(IntTensor, d10::d1_)
(* return FloatTensor with same shape, string literal not allowed *)
| Float_Of(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | STRING_Tensor | VAR_Tensor -> make_err (invalid_type "Float_Of() function") 
    ) in CheckTup(FloatTensor, d10::d1_)
(* return FloatTensor with same shape, string literal not allowed *)
| Floor(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | STRING_Tensor | VAR_Tensor -> make_err (invalid_type "Floor() function") 
    ) in CheckTup(FloatTensor, d10::d1_)
(* return FloatTensor with same shape, string literal not allowed *)
| Ceil(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | STRING_Tensor | VAR_Tensor -> make_err (invalid_type "Ceil() function") 
    ) in CheckTup(FloatTensor, d10::d1_)
(* return FloatTensor with same shape, string literal not allowed *)
| Round(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | STRING_Tensor | VAR_Tensor -> make_err (invalid_type "Round() function") 
    ) in CheckTup(FloatTensor, d10::d1_)
(* return FloatTensor with same shape, string literal not allowed *)
| Abs(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | STRING_Tensor | VAR_Tensor -> make_err (invalid_type "Abs() function") 
    ) in CheckTup(FloatTensor, d10::d1_)
(* return FloatTensor with same shape, string literal not allowed *)
| Log(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | STRING_Tensor | VAR_Tensor -> make_err (invalid_type "Log() function") 
    ) in CheckTup(FloatTensor, d10::d1_)
(* return FloatTensor with same shape, string literal not allowed *)
| Inverse(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | STRING_Tensor | VAR_Tensor -> make_err (invalid_type "Inverse() function") 
    ) in CheckTup(FloatTensor, d10::d1_)
(* return FloatTensor with same shape, string literal not allowed *)
| Svd(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | STRING_Tensor | VAR_Tensor -> make_err (invalid_type "Svd() function") 
    ) in CheckTup(FloatTensor, d10::d1_)
(* return FloatTensor, Ax = b, A=mxn, b=mx1, return x=nx1 *)
| Solve(e1, e2) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e1) in 
  let CheckTup(t2, d20:d2_) = checkexpr(e2) in
  let t3 = 
    (match t1, t2 with 
      STRING_Tensor, _ -> make_err (invalid_type "Solve() function")
    | _, STRING_Tensor -> make_err (invalid_type "Solve() function")
    | VAR_Tensor, _ -> make_err (invalid_type "Solve() function")
    | _, VAR_Tensor -> make_err (invalid_type "Solve() function")
    | _, _ -> FLOAT_Tensor) in 
    if List.length(d1_) <> 2 && List.length(d2_) <> 1 then make_err (invalid_dim "Solve() function only supports 2-dim tensor for first parameter and 1-dim tensor for second parameter")
    else if List.hd(d1_) <> List.hd(d2_) then make_err (invalid_dim "mismatch of the dimension between two tensor")
    else if List.length(d1_) = 2 && List.length(d2_) = 1 && List.hd(d1_) = List.hd(d2_) then CheckTup(t3, -1::List.td(d1_))
    else make_err dummy_error
(* return FloatTensor with same shape, string literal not allowed *)
| Eig(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | STRING_Tensor | VAR_Tensor -> make_err (invalid_type "Eig() function") 
    ) in
    if List.length(d1_) <> 2 then make_err (invalid_dim "Eig() function only supports 2-dim tensor")
    else if List.hd(d1_) <> List.tl(d1_) then make_err (invalid_dim "Eig() function only supports square tensor")
    else CheckTup(FloatTensor, d10::d1_)
(* return FloatTensor with same shape, string literal not allowed *)
| Eigv(e) -> 
  let CheckTup(t1, d10:d1_) = checkexpr(e) in 
    (match t1 with 
      | STRING_Tensor | VAR_Tensor -> make_err (invalid_type "Eigv() function") 
    ) in
    if List.length(d1_) <> 2 then make_err (invalid_dim "Eig() function only supports 2-dim tensor")
    else if List.hd(d1_) <> List.tl(d1_) then make_err (invalid_dim "Eig() function only supports square tensor")
    else CheckTup(FloatTensor, d10::d1_)



                      