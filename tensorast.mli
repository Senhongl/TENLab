type tensortype = INT_Tensor | FLOAT_Tensor

type dimtype = TensorTup of tensortype * int * int list | VoidTup

type literal =
  IntLit of int
| FloatLit of float

type bop = 
(* Arithmetic operators*)
Add | Sub | Mul | DotMul | Div | FloorDiv | Pow | DotPow | Mod

(* Relational operators *)
| Eq | Geq | Gt | Leq | Lt | Neq

(* Logical operators *)
(* | And | Or *)

type uop = Transpose

type tensor =
  LRTensor of tensor
| NPTensor of tensor
| LRTensors of tensor * tensor
| NPTensors of tensor * tensor
| Tensor0 of literal

type expr =
  Binop of expr * bop * expr
| Unop of expr * uop
| Tensor of tensor