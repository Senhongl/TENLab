type tensortype = INT_Tensor | FLOAT_Tensor

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

type expr =
  Binop of expr * op * expr
| Tensor of tensor