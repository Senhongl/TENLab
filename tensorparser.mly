%{ open Tensorast %}

%token L C R ADD MUL EOF VAR ASSGN SEP
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> ID

%left ADD
%left MUL
%right ASSGN

/*%start stmts
%type <Tensorast.stmt list> stmts
%type <Tensorast.stmt> stmt*/
%start expr
%type <Tensorast.expr> expr
%type <Tensorast.asexpr> asexpr
%type <Tensorast.tensor> tensor
%type <Tensorast.tensor> n_tensor

%%
/*stmts:
    { [] }
  | stmt SEP stmts { $1::$3 }
;
stmt:
    expr { Expr($1) }
  | asexpr ASSGN expr { Assign($1, $3) }
;*/
expr:
    expr ADD expr { Binop($1, Add, $3) }
  | expr MUL expr { Binop($1, Mul, $3) }
  | VAR tensor { VarTs($2) }
  | tensor { Tensor($1) }
  | asexpr { ASexpr($1) }
;
asexpr:
    ID { Ident($1) }
  | ID tensor { Idind($1, $2) }
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