open Tensorast

let rec print_tensor = function
  Tensor0(x) -> print_int(x)
| LRTensor(x) -> print_char('['); print_tensor(x); print_char(']')
| NPTensor(x) -> print_tensor(x)
| LRTensors(x1, x2) -> 
    print_char('['); 
    print_tensor(x1); print_string(", "); print_tensor(x2);
    print_char(']')
| NPTensors(x1, x2) -> print_tensor(x1); print_string(", "); print_tensor(x2)

let rec equal_dim x1 x2=
  match x1, x2 with
    x10::x1_, x20::x2_ -> if (x10 != x20) then false else equal_dim x1_ x2_
  | [], [] -> true
  | _, [] -> false
  | [], _ -> false

exception E of string

let rec tensor_dim = function
  Tensor0(x) -> [-1]
| LRTensor(x) -> 
    (match tensor_dim(x) with
      x0::x_ -> -1::-x0::x_
    | [] -> raise (E "ought not occur"))
| NPTensor(x) -> tensor_dim(x)
| LRTensors(x1, x2) -> 
    let x1 = tensor_dim(x1) in
    let x2 = tensor_dim(x2) in
      (match x1, x2 with
        x10::x1_, x20::x2_ -> 
          if equal_dim x1_ x2_ then -1::-(x10 + x20)::x1_
          else raise (E "invalid dim")
      | [], _ -> raise (E "ought not occur")
      | _, [] -> raise (E "ought not occur"))
| NPTensors(x1, x2) -> 
    let x1 = tensor_dim(x1) in
    let x2 = tensor_dim(x2) in
      (match x1, x2 with
        x10::x1_, x20::x2_ -> 
          if equal_dim x1_ x2_ then (x10 + x20)::x1_
          else raise (E "invalid dim")
      | [], _ -> raise (E "ought not occur")
      | _, [] -> raise (E "ought not occur"))

let rec print_list = function 
  [] -> print_endline(")")
| e::l -> if e < 0 then (print_char('('); print_list l)
          else (print_int e ; print_string ", " ; print_list l)

let a =
  let lexbuf = Lexing.from_channel stdin in
  let tensor = Tensorparser.tensor Tensorscanner.tokenize lexbuf in
    print_tensor tensor; print_endline("");
    let dims = tensor_dim tensor in print_list dims