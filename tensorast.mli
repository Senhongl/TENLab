type tensor =
    LRTensor of tensor
  | NPTensor of tensor
  | LRTensors of tensor * tensor
  | NPTensors of tensor * tensor
  | Tensor0 of int