(************ Abstract Syntax Tree types for TENLab ************)


type bop =
(* Arithmetic operators*)
Add | Sub | Mul | DotMul | Div | Pow | DotPow | Mod | FlrDiv

(* Relational operators *)
| Eq | Geq | Gt | Leq | Lt | Neq

(* Logical operators *)
| And | Or

type typ = Int | Float | String | Void

(* Unary operators *)
type uop = Not | Neg | Transpose

(* Literal *)
type literal =
  IntLit of int
| FloatLit of float
| StringLit of string

type operator_name =
  AddSymbol
| MinusSymbol
| MulSymbol

type expr =
  Lit of literal
| Id of string
| IntTensor of expr
| FloatTensor of expr
| VarTensor of expr
| LRTensor of expr
| NPTensor of expr
| LRTensors of expr * expr
| NPTensors of expr * expr
| Tensor0 of expr
| Binop of expr * bop * expr
| Unop of uop * expr
| Range of expr * expr * expr
(* Keyword expression *)
| Return of expr
| Break
| Continue
| Exit of expr
(* Built-in functions *)
| Print of expr
| Shape of expr
| Cat of expr * expr * expr
| Any of expr
| All of expr
| Sum of expr
| Ones of expr
| Zeros of expr
| Len of expr
| Int_Of of expr
| Float_Of of expr
| Floor of expr
| Ceil of expr
| Round of expr
| Abs of expr
| Log of expr
| Inverse of expr
| Solve of expr * expr
| Svd of expr
| Eig of expr
| Eigv of expr

| FuncCall of expr * expr list
| Assign of expr * expr

(* Statements *)
type stmt =
  EmptyStmt
| Expr of expr
| FuncSign of expr * expr list
| FuncDecl of stmt * stmt list
| Tdecl of string list * stmt
| IfStmt of expr * stmt list * stmt list
| ForStmt of expr * expr * stmt list
| WhileStmt of expr * stmt list
(* Parallel Environment *)
| PEDecl of expr * stmt list
| PEInvoke of expr
| POSign of string * expr list
| ParallelOperator of stmt * stmt
| MapReduce of stmt list * stmt
| MapFunc of expr * stmt list
| ReduceFunc of stmt list


let string_of_bop = function
  Add -> "+"
| Sub -> "-"
| Mul -> "*"
| DotMul -> ".*"
| Div -> "/"
| Pow -> "^"
| DotPow -> ".^"
| Mod -> "%"
| FlrDiv -> "//"
| Eq -> "=="
| Geq -> ">="
| Gt -> ">"
| Leq -> "<="
| Lt -> "<"
| Neq -> "!="
| And -> "&&"
| Or -> "||"


let string_of_uop = function
  Not -> "!"
| Neg -> "-"
| Transpose -> "'"

let string_of_lit = function
  IntLit(l) -> string_of_int l
| FloatLit(l) -> string_of_float l
| StringLit(l) -> "\"" ^ l ^ "\""

let rec string_of_expr = function
  Lit(l) -> string_of_lit l
| Id(str1) -> str1
| Binop(e1, bop, e2) -> string_of_expr e1 ^ " " ^ string_of_bop bop ^ " " ^ string_of_expr e2
| Unop(uop, e1) -> string_of_uop uop ^ " " ^ string_of_expr e1
| Range(e1, e2, e3) -> string_of_expr e1 ^ ":" ^ string_of_expr e2 ^ ":" ^ string_of_expr e3
| Return(e1) -> "return " ^ string_of_expr e1 ^ "\n"
| Break -> "break\n"
| Continue -> "continue\n"
| Exit(e1) -> "exit" ^ string_of_expr e1 ^ "\n"

(* built-in functions *)
| Print(e1) -> "print(" ^ string_of_expr e1 ^ ")"
| Shape(e1) -> "shape(" ^ string_of_expr e1 ^ ")"
| Cat(e1, e2, e3) -> "cat(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ string_of_expr e3 ^ ")"
| Any(e1) -> "any (" ^ string_of_expr e1 ^ ")"
| All(e1) -> "all (" ^ string_of_expr e1 ^ ")"
| Sum(e1) -> "sum (" ^ string_of_expr e1 ^ ")"
| Ones(e1) -> "ones (" ^ string_of_expr e1 ^ ")"
| Zeros(e1) -> "zeros (" ^ string_of_expr e1 ^ ")"
| Len(e1) -> "len (" ^ string_of_expr e1 ^ ")"
| Int_Of(e1) -> "int_of (" ^ string_of_expr e1 ^ ")"
| Float_Of(e1) -> "float_of (" ^ string_of_expr e1 ^ ")"
| Floor(e1) -> "floor (" ^ string_of_expr e1 ^ ")"
| Ceil(e1) -> "ceil (" ^ string_of_expr e1 ^ ")"
| Round(e1) -> "round (" ^ string_of_expr e1 ^ ")"
| Abs(e1) -> "abs(" ^ string_of_expr e1 ^ ")"
| Log(e1) -> "log (" ^ string_of_expr e1 ^ ")"
| Inverse(e1) -> "inverse (" ^ string_of_expr e1 ^ ")"
| Solve(e1, e2) -> "solve (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
| Svd(e1) -> "svd (" ^ string_of_expr e1 ^ ")"
| Eig(e1) -> "eig (" ^ string_of_expr e1 ^ ")"
| Eigv(e1) -> "Eigv (" ^ string_of_expr e1 ^ ")"
| FuncCall(e1, e2) -> (string_of_expr e1 ^ " = " ^ (String.concat "," (List.map string_of_expr e2)))
| Assign(e1, e2) -> (string_of_expr e1 ^ " = " ^ string_of_expr e2)

| Tensor0(e1) -> string_of_expr(e1)
| LRTensor(e1) -> "[" ^ string_of_expr e1 ^ "]"
| NPTensor(e1) -> string_of_expr(e1)
| LRTensors(e1, e2) -> "[" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ "]"
| NPTensors(e1, e2) -> string_of_expr e1 ^ ", " ^ string_of_expr e2
| IntTensor(e1) -> "int(" ^ string_of_expr e1 ^ ")"
| FloatTensor(e1) -> "float(" ^ string_of_expr e1 ^ ")"
| VarTensor(e1) -> "var(" ^ string_of_expr e1 ^ ")"

let rec string_of_stmt = function
  EmptyStmt -> ""
| Expr(e1) -> string_of_expr e1 ^ ";\n"
| FuncSign(e1, e2) -> string_of_expr e1 ^ "(" ^ (String.concat "," (List.map string_of_expr e2)) ^ ")" ^ "\n"
| FuncDecl(s1, s2) -> "def " ^ string_of_stmt s1 ^ "{\n" ^ String.concat "," (List.map string_of_stmt s2) ^ "}\n"
| Tdecl(str1, s1) -> String.concat "," str1 ^ " = " ^ string_of_stmt s1 ^ "\n"
| IfStmt(e1, s2, s3) -> (match s3 with
    | [EmptyStmt] -> "if (" ^ string_of_expr e1 ^ ")\n{\n" ^ String.concat "," (List.map string_of_stmt s2) ^ "}\n"
    | _ -> "if (" ^ string_of_expr e1 ^ ")\n{\n" ^ String.concat "," (List.map string_of_stmt s2) ^ "} else {" ^ String.concat "," (List.map string_of_stmt s3) ^ "\n}\n")
| ForStmt(e1, e2, s1) -> "for (" ^ string_of_expr e1 ^ " in " ^ string_of_expr e2 ^ ") {\n" ^ String.concat "," (List.map string_of_stmt s1) ^ "\n}\n"
| WhileStmt(e1, s1) -> "while (" ^ string_of_expr e1 ^ ") {\n" ^ String.concat "," (List.map string_of_stmt s1) ^ "\n}\n"
(* Parallel Environment *)
| PEDecl(e1, s1) -> "parallel_define " ^ string_of_expr e1 ^ "{\n" ^ String.concat "," (List.map string_of_stmt s1) ^ "}\n"
| PEInvoke(e1) -> "using " ^ string_of_expr e1
| POSign(str1, e1) -> "    overload " ^ str1 ^ " (" ^ String.concat "," (List.map string_of_expr e1) ^ ") "
| ParallelOperator(s1, s2) -> string_of_stmt s1 ^ "\n" ^ string_of_stmt s2
| MapReduce(map, reduce) -> String.concat "\n" (List.map string_of_stmt map) ^ "\n" ^ string_of_stmt reduce
| MapFunc(e1, s1)-> "        map " ^ string_of_expr e1 ^ "{\n" ^ String.concat "\n" (List.map string_of_stmt s1) ^ "}"
| ReduceFunc(s1)-> "        reduce {\n" ^ String.concat "\n" (List.map string_of_stmt s1) ^ "}"

and string_of_program l = String.concat "" (List.map string_of_stmt l) ^ "\n"
