open Tensorast
open Tensorsast

(*let rec print_expr = function
  Lit(x) -> (match x with
    IntLit(x) -> print_int(x)
  | FloatLit(x) -> print_float(x))

| Binop(x1, op, x2) -> 
    print_expr(x1);
    (match op with 
      Add -> print_string(" + ")
    | Mul -> print_string(" * "));
    print_expr(x2)

| Tensor0(x) -> print_expr(x)
| LRTensor(x) -> print_char('['); print_expr(x); print_char(']')
| NPTensor(x) -> print_expr(x)
| LRTensors(x1, x2) -> 
    print_char('['); 
    print_expr(x1); print_string(", "); print_expr(x2);
    print_char(']')
| NPTensors(x1, x2) -> print_expr(x1); print_string(", "); print_expr(x2)*)

let rec equal_dim d1 d2 =
  match d1, d2 with
    d10::d1_, d20::d2_ -> if d10 <> d20 then false else equal_dim d1_ d2_
  | [], [] -> true
  | _, [] -> false
  | [], _ -> false

(*let trunc_dim d =
  match d with
    d0::d_ -> d_
  | [] -> []*)

exception E of string

(* tensor -> dimtype *)
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
let rec check_expr = function
  Binop(x1, bop, x2) -> (SVoidTup, SBinop(check_expr(x1), bop, check_expr(x2)))
| Unop(x, uop) -> (SVoidTup, SUnop(check_expr(x), uop))
| Tensor(x) -> match check_tensor(x) with 
  | (TensorTup(t, n, d::d_), y) -> (STensorTup(t, n, Array.of_list d_), STensor(y))
  | (_, _) -> raise (E "ought not occur")

(*let rec print_dims = function 
  [] -> ()
| e::l -> print_int e; print_string ", "; print_dims l

let print_type = function
  INT_Tensor -> print_endline("int")
| FLOAT_Tensor -> print_endline("float")

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Tensorparser.expr Tensorscanner.tokenize lexbuf in
  let semant = check_expr ast in 
      match semant with
        TensorTup(ttype, ndim, dims) ->
          print_type ttype; print_int(ndim); print_endline(""); print_dims dims
      | VoidTup -> print_endline("Void *")*)