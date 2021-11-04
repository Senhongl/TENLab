%{ open Ast %}

%token NEWLINE EOF
// arithmetic operators
%token PLUS SUBTRACT MULTIPLICATION DOT_MULTIPLICATION DIVIDE POWER DOT_POWER TRANSPOSE MOD FLOOR_DIVIDE NEG
// relational operators
%token IS_EQUAL IS_GEQ IS_GT IS_LEQ IS_LT IS_NOT_EQUAL
// logical operators
%token AND OR NOT
// parentheses and brackets
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS LEFT_CURLY_BRACKET RIGHT_CURLY_BRACKET LEFT_SQUARE_BRACKET RIGHT_SQUARE_BRACKET
// delimiters
%token COMMA COLONS
// assignment
%token ASSIGNMENT
// keywords
// TODO: support special keywords, e.g., read, print, shape, cat
// TODO: string or char? 
%token IF ELIF ELSE FOR WHILE IN CONTINUE BREAK RETURN EXIT DEFINE INT FLOAT STRING
%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token <float> FLOAT_LITERAL
%token <string> IDENTIFIER

%nonassoc COLON
%nonassoc COMMA
%nonassoc NOELSE
// does parentheses or brackets matter?
%right ASSIGNMENT
%left OR
%left AND
%left IS_EQUAL IS_NOT_EQUAL
%left IS_GEQ IS_GT IS_LEQ IS_LT
%left PLUS SUBTRACT
%left MULTIPLICATION DOT_MULTIPLICATION DIVIDE MOD FLOOR_DIVIDE
%right POWER DOT_POWER
%left TRANSPOSE
%right NOT NEG

%start main
%type <Ast.stmt list> main

%%

main:
  stmts EOF { $1 }


/***************************************************************************************
                                Statements
 ***************************************************************************************/

stmts:
| { [] }
| NEWLINE stmts { $2 }
| stmt stmts { $1::$2 }

/*
 * A TENLab file should consis of a bunch of statements.
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
| DEFINE func_signature stmt_body { FuncDecl($2, $3) }
| func_call { $1 }
| expr { Expr($1) }
| params ASSIGNMENT stmt { Tdecl($1, $3) } 
| RETURN params { Return($2) }
| BREAK { Break }
| CONTINUE { Continue }
| EXIT { Exit }
| IF LEFT_PARENTHESIS stmt RIGHT_PARENTHESIS stmt_body %prec NOELSE { IfStmt($3, $5, [EmptyStmt]) }
| IF LEFT_PARENTHESIS stmt RIGHT_PARENTHESIS stmt_body ELSE stmt_body { IfStmt($3, $5, $7) }
| FOR LEFT_PARENTHESIS IDENTIFIER IN expr RIGHT_PARENTHESIS stmt_body { ForStmt($3, $5, $7) }
| WHILE LEFT_PARENTHESIS expr RIGHT_PARENTHESIS stmt_body { WhileStmt($3, $5) }

stmt_body:
| NEWLINE stmt_body { $2 }
| LEFT_CURLY_BRACKET stmts RIGHT_CURLY_BRACKET { $2 }
// | stmts stmt_body { $1 }
// | NEWLINE stmt_body { $2 }
// | stmt_body NEWLINE { $1 }
// | LEFT_CURLY_BRACKET stmt_body { $2 }
// | stmt_body RIGHT_CURLY_BRACKET { $1 }


/***************************************************************************************
                        Function Call
 ***************************************************************************************/

func_signature: IDENTIFIER LEFT_PARENTHESIS params RIGHT_PARENTHESIS { FuncSign($1, $3) }

/* We support the following form of function call:
 *      (i)  call the function directly
 *      (ii) call the function and assign the return value to variable(s) 
 */
// TODO: support a, b = foo(), foo()? *)
func_call: 
| func_signature { $1 }
| params ASSIGNMENT func_call { FuncCall($1, $3) }

params:
  { [] }
| param {List.rev $1 }

param:
// the list of params could either be a list of id or a list of expr
  IDENTIFIER { [$1] }
| IDENTIFIER COMMA param { $1 :: $3 }
// TODO: support a list of expr? *)
// | expr COMMA expr { $2::$1 }

/***************************************************************************************
                Tensor Declaration & Assignment
 ***************************************************************************************/
/* We support the following forms of tensor declaration & assignment:
 *       (i)   From exisiting data, e.g., A = 1 TODO: support tensor.
 *       (ii)  TODO: From shape and values with built-in function
 *       (iii) Create numerical range, e.g., A = 0:3:1
 *       (iv)  From exisiting params, e.g., B = A
 */
// tdecl:
// TODO: support A, B = 1, 2?
// | expr { [$1] }
// | expr COMMA tdecl { $2::$1 }
// | params ASSIGNMENT expr { Tdecl($1, $3) } 


/***************************************************************************************
        All possible expressions, including binary expression and unary expression
 ***************************************************************************************/
expr:
// Primitive data type
| INT_LITERAL { Lit(IntLit($1)) }
| FLOAT_LITERAL { Lit(FloatLit($1)) }
| STRING_LITERAL { Lit(StringLit($1)) }
| IDENTIFIER { Lit(StringLit($1)) }
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
| NEG expr { Unop(Neg, $2) }
| expr TRANSPOSE { Unop(Transpose, $1) }
// A special expression, numerical range. *)
/* TODO: it only support the numerical initialization, i.e., 0:3:1.
 *       however, we might want to support 0:shape(A):1. 
 */
| expr COLON expr COLON expr { Range($1, $3, $5) }
