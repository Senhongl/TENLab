%{ open Tensorast %}

%token L C R ADD SUB MUL DOTMUL DIV FLOORDIV POW DOTPOW MOD TRANSPOSE EOF 
%token IS_EQUAL IS_GEQ IS_GT IS_LEQ IS_LT IS_NOT_EQUAL
%token AND OR LNOT
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL

%left AND OR
%left IS_EQUAL IS_GEQ IS_GT IS_LEQ IS_LT IS_NOT_EQUAL
%left ADD, SUB, MOD
%left MUL, DOTMUL, DIV, FLOORDIV, POW, DOTPOW
%right LNOT
%left TRANSPOSE

%start expr
%type <Tensorast.expr> expr
%type <Tensorast.tensor> tensor
%type <Tensorast.tensor> n_tensor

%%
expr:
    /*  */
    expr ADD expr { Binop($1, Add, $3) }
  | expr SUB expr { Binop($1, Sub, $3) }
  | expr MUL expr { Binop($1, Mul, $3) }
  | expr DOTMUL expr { Binop($1, DotMul, $3) }
  | expr DIV expr { Binop($1, Div, $3) }
  | expr FLOORDIV expr { Binop($1, FloorDiv, $3) }
  | expr POW expr { Binop($1, Pow, $3) }
  | expr DOTPOW expr { Binop($1, DotPow, $3) }
  | expr MOD expr { Binop($1, Mod, $3) }

  | expr IS_EQUAL expr { Binop($1, Eq, $3) }
  | expr IS_GEQ expr { Binop($1, Geq, $3) }
  | expr IS_GT expr { Binop($1, Gt, $3) }
  | expr IS_LEQ expr { Binop($1, Leq, $3) }
  | expr IS_LT expr { Binop($1, Lt, $3) }
  | expr IS_NOT_EQUAL expr { Binop($1, Neq, $3) }

  | expr AND expr { Binop($1, And, $3) }
  | expr OR expr { Binop($1, Or, $3) }
  | LNOT expr { Lunop(Not, $2) }

  | expr TRANSPOSE { Unop($1, Transpose) }
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