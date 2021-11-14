%{ open Tensorast %}

%token L C R EOF
%token <int> NUM

%start tensor
%type <Tensorast.tensor> tensor
%type <Tensorast.tensor> n_tensor

%%
tensor:
    L tensor C n_tensor R { LRTensors($2, $4) }
  | L tensor R { LRTensor($2) }
  | NUM { Tensor0($1) }
;
n_tensor:
    tensor C n_tensor { NPTensors($1, $3) }
  | tensor { NPTensor($1) }
;