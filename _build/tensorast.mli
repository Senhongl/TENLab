type literal =
  IntLit of int
| FloatLit of float

type expr =
  Lit of literal
| IntTensor of expr
| FloatTensor of expr
| LRTensor of expr
| NPTensor of expr
| LRTensors of expr * expr
| NPTensors of expr * expr
| Tensor0 of expr