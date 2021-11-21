%{ open Tensorast %}

%token L C R ADD MUL EOF
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL

%left ADD
%left MUL

%start expr
%type <Tensorast.expr> expr
%type <Tensorast.tensor> tensor
%type <Tensorast.tensor> n_tensor

%%
expr:
    expr ADD expr { Binop($1, Add, $3) }
  | expr MUL expr { Binop($1, Mul, $3) }
  | tensor { Tensor($1) }
;
tensor:
    L tensor C n_tensor R { LRTensors($2, $4) }
  | L tensor R { LRTensor($2) }
  | INT_LITERAL { Tensor0(IntLit($1)) }
  | FLOAT_LITERAL { Tensor0(FloatLit($1)) }
;
n_tensor:
    tensor C n_tensor { NPTensors($1, $3) }
  | tensor { NPTensor($1) }
;