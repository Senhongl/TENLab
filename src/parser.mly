%{ open Ast %}

%token SEP EOF
// arithmetic operators
%token PLUS SUBTRACT MULTIPLICATION DOT_MULTIPLICATION DIVIDE POWER DOT_POWER TRANSPOSE MOD FLOOR_DIVIDE
// relational operators
%token IS_EQUAL IS_GEQ IS_GT IS_LEQ IS_LT IS_NOT_EQUAL
// logical operators
%token AND OR NOT
// parentheses and brackets
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS LEFT_CURLY_BRACKET RIGHT_CURLY_BRACKET LEFT_SQUARE_BRACKET RIGHT_SQUARE_BRACKET
// delimiters
%token COMMA COLON
// assignment
%token ASSIGNMENT
// keywords
// TODO: support special keywords, e.g., read, print, shape, cat
// build-in functions
%token ANY ALL SUM ONES ZEROS LEN INT_OF FLOAT_OF FLOOR CEIL ROUND ABS LOG INVERSE SOLVE SVD EIG EIGV PRINT SHAPE CAT

// TODO: string or char?
%token IF ELIF ELSE FOR WHILE IN CONTINUE BREAK RETURN EXIT DEFINE INT FLOAT STRING VAR PARALLEL_DEFINE OVERLOAD MAP REDUCE USING RETURN END
%token <string> OPERATOR_INDICATOR
%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token <float> FLOAT_LITERAL
%token <string> IDENTIFIER

%left SEP
%nonassoc RIGHT_SQUARE_BRACKET RIGHT_CURLY_BRACKET
%nonassoc LEFT_SQUARE_BRACKET LEFT_CURLY_BRACKET
%nonassoc COLON
%nonassoc COMMA
%nonassoc NOELSE
%left RETURN
%right ASSIGNMENT
%left OR
%left AND
%left IS_EQUAL IS_NOT_EQUAL
%left IS_GEQ IS_GT IS_LEQ IS_LT
%left PLUS SUBTRACT
%left MULTIPLICATION DOT_MULTIPLICATION DIVIDE MOD FLOOR_DIVIDE
%right POWER DOT_POWER
%left TRANSPOSE
%right NOT

%nonassoc RIGHT_PARENTHESIS 
%nonassoc LEFT_PARENTHESIS 

%start main
%type <Ast.program> main

%%

main:
  pes normal_stmts EOF { ($1, $2) }


/***************************************************************************************
                                Statements
 ***************************************************************************************/

normal_stmts: 
| { [] }
| normal_stmt normal_stmts { $1::$2 }

func_stmts: 
| { [] }
| func_stmt func_stmts { $1::$2 }

loop_stmts: 
| { [] }
| loop_stmt loop_stmts { $1::$2 }

/*
 * A TENLab file should consist of a bunch of statements.
 * Here we define all the possible statements:
 * (i)    a function declaration or function call
 * (ii)   an exprssion inside/outside the body of function
 * (iii)  tensor declaration or assignment
 * (iv)   a return/break/continue/exit statement
 * (v)    if statement/if-else statement
 * (vi)   TODO: if-elif statement
 * (vii)  for statement
 * (viii) while statement
 * (ix)   TODO: more statments, e.g., built-in function?
 */
stmt:
| expr SEP { Expr($1) }
// TODO: support a, b = 1, 2?
| IDENTIFIER ASSIGNMENT expr SEP { Assign($1, $3) }
| USING IDENTIFIER { PEInvoke($2) }
| END IDENTIFIER { PEEND($2) }
| DEFINE IDENTIFIER LEFT_PARENTHESIS params RIGHT_PARENTHESIS func_stmt_body { FuncDecl($2, $4, $6) }
| IF LEFT_PARENTHESIS expr RIGHT_PARENTHESIS stmt_body %prec NOELSE { IfStmt($3, $5, [EmptyStmt]) }
| IF LEFT_PARENTHESIS expr RIGHT_PARENTHESIS stmt_body ELSE stmt_body { IfStmt($3, $5, $7) }
| FOR LEFT_PARENTHESIS IDENTIFIER IN expr RIGHT_PARENTHESIS loop_stmt_body { ForStmt($3, $5, $7) }
| WHILE LEFT_PARENTHESIS expr RIGHT_PARENTHESIS loop_stmt_body { WhileStmt($3, $5) }
| EXIT LEFT_PARENTHESIS expr RIGHT_PARENTHESIS SEP { Exit($3) }

normal_stmt:
| stmt { $1 }
| RETURN expr SEP { raise(Failure ("Return outside functions")) }
| BREAK SEP { raise(Failure ("Break outside loops"))  }
| CONTINUE SEP { raise(Failure ("Continue outside loops"))  }

func_stmt:
| stmt { $1 }
| RETURN expr SEP { Return($2) }
| BREAK SEP { raise(Failure ("Break outside loops"))  }
| CONTINUE SEP { raise(Failure ("Continue outside loops"))  }

loop_stmt:
| stmt { $1 }
| BREAK SEP { Break }
| CONTINUE SEP { Continue }
| RETURN expr SEP { raise(Failure ("Return outside functions")) }

stmt_body: LEFT_CURLY_BRACKET normal_stmts RIGHT_CURLY_BRACKET { $2 }

func_stmt_body: LEFT_CURLY_BRACKET func_stmts RIGHT_CURLY_BRACKET { $2 }

loop_stmt_body: LEFT_CURLY_BRACKET loop_stmts RIGHT_CURLY_BRACKET { $2 }


/***************************************************************************************
                        Function Call
 ***************************************************************************************/

/* func_signature: IDENTIFIER LEFT_PARENTHESIS params RIGHT_PARENTHESIS { FuncSign($1, $3) } */

/* We support the following form of function call:
 *      (i)  call the function directly
 *      (ii) call the function and assign the return value to variable(s)
 */
// TODO: support a, b = foo(), foo()? *)
func_call: IDENTIFIER LEFT_PARENTHESIS exprs RIGHT_PARENTHESIS { FuncCall(FId($1), $3) }

exprs:
| { [] }
| expr_list { $1 }

expr_list:
| expr { [$1] }
| expr COMMA expr_list { $1 :: $3 }

params:
| { [] }
| param_list { $1 }

param_list:
| IDENTIFIER { [$1] }
| IDENTIFIER COMMA param_list { $1 :: $3 }

/***************************************************************************************
                    Parallel Environment
 ***************************************************************************************/

pes:
| { [] }
| pe pes { $1::$2 }

pe: PARALLEL_DEFINE IDENTIFIER LEFT_CURLY_BRACKET po_list RIGHT_CURLY_BRACKET { ($2, $4) }

po_list:
| po { [$1] }
| po po_list { $1 :: $2 }

po: OVERLOAD OPERATOR_INDICATOR LEFT_PARENTHESIS params RIGHT_PARENTHESIS LEFT_CURLY_BRACKET normal_stmts map_funcs reduce_func RIGHT_CURLY_BRACKET
{ {
  operator = $2;
  params = $4;
  headstmt = $7;
  mapfuncs = $8;
  reducefunc = $9;
} }

map_funcs:
| MAP IDENTIFIER func_stmt_body { [($2, $3)] }
| MAP IDENTIFIER func_stmt_body map_funcs { ($2, $3) :: $4 }

reduce_func: REDUCE func_stmt_body { $2 }

/***************************************************************************************
        All possible expressions, including binary expression and unary expression
 ***************************************************************************************/
expr:
// multi-dim data type
// TODO: String tensor?
| tensor { Tensor($1) }
// Indentifier
| IDENTIFIER { Id($1) }
// Expression within parenthesis
| LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { $2 }
// Binary expression
| expr PLUS expr { Binop($1, Add, $3) }
| expr SUBTRACT expr { Binop($1, Sub, $3) }
| expr MULTIPLICATION expr { Binop($1, Mul, $3) }
| expr DOT_MULTIPLICATION expr { Binop($1, DotMul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| expr POWER expr { Binop($1, Pow, $3) }
| expr DOT_POWER expr { Binop($1, DotPow, $3) }
| expr MOD expr { Binop($1, Mod, $3) }
| expr FLOOR_DIVIDE expr { Binop($1, FlrDiv, $3) }
| expr IS_EQUAL expr { Binop($1, Eq, $3) }
| expr IS_GEQ expr { Binop($1, Geq, $3) }
| expr IS_GT expr { Binop($1, Gt, $3) }
| expr IS_LEQ expr { Binop($1, Leq, $3) }
| expr IS_LT expr { Binop($1, Lt, $3) }
| expr IS_NOT_EQUAL expr { Binop($1, Neq, $3) }
| expr AND expr { Binop($1, And, $3) }
| expr OR expr { Binop($1, Or, $3) }
// Unary expression
| NOT expr { Unop(Not, $2) }
| SUBTRACT expr { Unop(Neg, $2) }
| expr TRANSPOSE { Unop(Transpose, $1) }
// A special expression, numerical range. *)
| expr COLON expr COLON expr { Range($1, $3, $5) }
// built-in functions
// TODO: necessary to do the syntax check?
| PRINT LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Print($3) }
| SHAPE LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Shape($3) }
| CAT LEFT_PARENTHESIS expr COMMA expr COMMA expr RIGHT_PARENTHESIS { Cat($3, $5, $7) }
| ANY LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Any($3) }
| ALL LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { All($3) }
| SUM LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Sum($3) }
| ONES LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Ones($3) }
| ZEROS LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Zeros($3) }
| LEN LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Len($3) }
| INT_OF LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Int_Of($3) }
| FLOAT_OF LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Float_Of($3) }
| FLOOR LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Floor($3) }
| CEIL LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Ceil($3) }
| ROUND LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Round($3) }
| ABS LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Abs($3) }
| LOG LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Log($3) }
| INVERSE LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Inverse($3) }
| SOLVE LEFT_PARENTHESIS expr COMMA expr RIGHT_PARENTHESIS { Solve($3, $5) }
| SVD LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Svd($3) }
| EIG LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Eig($3) }
| EIGV LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { Eigv($3) }
// function call
| func_call { $1 }

// tensor
tensor:
    LEFT_SQUARE_BRACKET tensor COMMA n_tensor RIGHT_SQUARE_BRACKET { LRTensors($2, $4) }
  | LEFT_SQUARE_BRACKET tensor RIGHT_SQUARE_BRACKET { LRTensor($2) }
  | LEFT_SQUARE_BRACKET RIGHT_SQUARE_BRACKET { LRTensor(EmptyTensor) }
  | INT_LITERAL { Tensor0(IntLit($1)) }
  | FLOAT_LITERAL { Tensor0(FloatLit($1)) }

n_tensor:
    tensor COMMA n_tensor { NPTensors($1, $3) }
  | tensor { NPTensor($1) }