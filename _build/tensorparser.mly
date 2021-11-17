%{ open Tensorast %}

%token L C R LP RP EOF INT FLOAT
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL

%start expr
%type <Tensorast.expr> expr
%type <Tensorast.expr> tensor
%type <Tensorast.expr> n_tensor

%%
expr:
| INT_LITERAL { Lit(IntLit($1)) }
| FLOAT_LITERAL { Lit(FloatLit($1)) }
| INT LP tensor RP { IntTensor($3) }
| FLOAT LP tensor RP { FloatTensor($3) }

tensor:
    L tensor C n_tensor R { LRTensors($2, $4) }
  | L tensor R { LRTensor($2) }
  | expr { Tensor0($1) }
;
n_tensor:
    tensor C n_tensor { NPTensors($1, $3) }
  | tensor { NPTensor($1) }
;