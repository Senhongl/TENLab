open Tensorast

let rec print_expr = function
  Lit(x) -> (match x with
    IntLit(x) -> print_int(x)
  | FloatLit(x) -> print_float(x))
| IntTensor(x) -> print_string("int("); print_expr(x); print_char(')')
| FloatTensor(x) -> print_string("float("); print_expr(x); print_char(')')
| Tensor0(x) -> print_expr(x)
| LRTensor(x) -> print_char('['); print_expr(x); print_char(']')
| NPTensor(x) -> print_expr(x)
| LRTensors(x1, x2) -> 
    print_char('['); 
    print_expr(x1); print_string(", "); print_expr(x2);
    print_char(']')
| NPTensors(x1, x2) -> print_expr(x1); print_string(", "); print_expr(x2)

let rec equal_dim d1 d2=
  match d1, d2 with
    d10::d1_, d20::d2_ -> if d10 <> d20 then false else equal_dim d1_ d2_
  | [], [] -> true
  | _, [] -> false
  | [], _ -> false

exception E of string

type tensortype = FLOAT_Tensor | INT_Tensor

type dimtype = CheckTup of tensortype * int list

let rec check_expr = function
  Lit(x) -> (match x with
    IntLit(x) -> CheckTup(INT_Tensor, [-1])
  | FloatLit(x) -> CheckTup(FLOAT_Tensor, [-1]))
| IntTensor(x) -> (match check_expr(x) with
    CheckTup(t, d) -> 
      if t = INT_Tensor then CheckTup(t, d) else raise (E "invalid type"))
| FloatTensor(x) -> (match check_expr(x) with
    CheckTup(t, d) -> 
      if t = FLOAT_Tensor then CheckTup(t, d) else raise (E "invalid type"))

| Tensor0(x) -> check_expr(x)
| LRTensor(x) -> 
    (match check_expr(x) with
      CheckTup(t, d0::d_) -> CheckTup(t, -1::-d0::d_)
    | CheckTup(_, []) -> raise (E "ought not occur"))
| NPTensor(x) -> check_expr(x)
| LRTensors(x1, x2) -> 
    let td1 = check_expr(x1) in
    let td2 = check_expr(x2) in
      (match td1, td2 with
        CheckTup(t1, d10::d1_), CheckTup(t2, d20::d2_) -> 
          if t1 = t2 && equal_dim d1_ d2_ then CheckTup(t1, -1::-(d10 + d20)::d1_)
          else if t1 <> t2 then raise (E "invalid type")
          else raise (E "invalid dim")
      | _, _ -> raise (E "ought not occur"))
| NPTensors(x1, x2) -> 
    let td1 = check_expr(x1) in
    let td2 = check_expr(x2) in
      (match td1, td2 with
        CheckTup(t1, d10::d1_), CheckTup(t2, d20::d2_) -> 
          if t1 = t2 && equal_dim d1_ d2_ then CheckTup(t1, (d10 + d20)::d1_)
          else if t1 <> t2 then raise (E "invalid type")
          else raise (E "invalid dim")
      | _, _ -> raise (E "ought not occur"))

let rec print_list = function 
  [] -> print_endline(")")
| e::l -> if e < 0 then (print_char('('); print_list l)
          else (print_int e; print_string ", "; print_list l)

let print_type = function
  INT_Tensor -> print_endline("int")
| FLOAT_Tensor -> print_endline("float")

let a =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Tensorparser.expr Tensorscanner.tokenize lexbuf in
    print_expr expr; print_endline("");
    let CheckTup(ttype, dims) = check_expr expr in 
      print_type ttype; print_list dims