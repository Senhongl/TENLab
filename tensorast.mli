type tensortype = INT_Tensor | FLOAT_Tensor(* | VAR_Tensor*)

type dimtype = TensorTup of tensortype * int * int list | VoidTup

type literal =
  IntLit of int
| FloatLit of float

type op = Add | Mul

type tensor =
  LRTensor of tensor
| NPTensor of tensor
| LRTensors of tensor * tensor
| NPTensors of tensor * tensor
| Tensor0 of literal

type asexpr = 
  Ident of string
| Idind of string * tensor

type expr =
  Binop of expr * op * expr
| Tensor of tensor
| VarTs of tensor
| ASexpr of asexpr

type stmt = 
  Expr of expr
| Assign of asexpr * expr