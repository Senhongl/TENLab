(************ Abstract Syntax Tree types for TENLab ************)


type bop =
(* Arithmetic operators*)
Add | Sub | Mul | DotMul | Div | Pow | DotPow | Mod | FlrDiv

(* Relational operators *)
| Eq | Geq | Gt | Leq | Lt | Neq

(* Logical operators *)
| And | Or

type tensortype = INT_Tensor | FLOAT_Tensor | STRING_Tensor | VAR_Tensor

type dimtype = TensorTup of tensortype * int * int list | VoidTup

(* Unary operators *)
type uop = Not | Neg | Transpose

(* Literal *)
type literal =
  IntLit of int
| FloatLit of float

type operator_name =
  AddSymbol
| MinusSymbol
| MulSymbol

type expr =
| FId of string
| Tensor of tensor
| VarTs of expr list
| ASexpr of asexpr
| StringLit of string
| Binop of expr * bop * expr
| Unop of uop * expr
| Range of expr * expr * expr
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

and asexpr = 
  Id of string
| Idind of string * expr list

(* Tensor *)
and tensor =
  EmptyTensor
| LRTensor of tensor
| NPTensor of tensor
| LRTensors of tensor * tensor
| NPTensors of tensor * tensor
| Tensor0 of literal

(* Statements *)
type stmt =
  EmptyStmt
| Expr of expr
| Assign of asexpr * expr
(* | FuncSign of string * string list *)
| FuncDecl of string * string list * stmt list
| IfStmt of expr * stmt list * stmt list
| ForStmt of string * expr * stmt list
| WhileStmt of expr * stmt list
| PEInvoke of string
| PEEnd of string
(* Keyword statement *)
| Return of expr
| Break
| Continue
| Exit of expr

type pofunc = {
  operator : string;
  params : string list;
  headstmt : stmt list;
  mapfuncs : (string * stmt list) list;
  reducefunc : stmt list;
}

type program = (string * pofunc list) list * stmt list


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
(*| StringLit(l) -> "\"" ^ l ^ "\""*)

let rec string_of_tensor = function
  EmptyTensor -> ""
| Tensor0(l) -> string_of_lit l
| LRTensor(t1) -> "[" ^ string_of_tensor t1 ^ "]"
| NPTensor(t1) -> string_of_tensor t1
| LRTensors(t1, t2) -> "[" ^ string_of_tensor t1 ^ ", " ^ string_of_tensor t2 ^ "]"
| NPTensors(t1, t2) -> string_of_tensor t1 ^ ", " ^ string_of_tensor t2

let rec string_of_asexpr = function
| Id(id) -> id

let rec string_of_expr = function
| FId(str1) -> str1
| Tensor(t1) -> string_of_tensor t1
| Binop(e1, bop, e2) -> string_of_expr e1 ^ " " ^ string_of_bop bop ^ " " ^ string_of_expr e2
| Unop(uop, e1) -> string_of_uop uop ^ " " ^ string_of_expr e1
| Range(e1, e2, e3) -> string_of_expr e1 ^ ":" ^ string_of_expr e2 ^ ":" ^ string_of_expr e3

(* built-in functions *)
| Print(e1) -> "print(" ^ string_of_expr e1 ^ ")"
| Shape(e1) -> "shape(" ^ string_of_expr e1 ^ ")"
| Cat(e1, e2, e3) -> "cat(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ "," ^ string_of_expr e3 ^ ")"
| Any(e1) -> "any(" ^ string_of_expr e1 ^ ")"
| All(e1) -> "all(" ^ string_of_expr e1 ^ ")"
| Sum(e1) -> "sum(" ^ string_of_expr e1 ^ ")"
| Ones(e1) -> "ones(" ^ string_of_expr e1 ^ ")"
| Zeros(e1) -> "zeros(" ^ string_of_expr e1 ^ ")"
| Len(e1) -> "len(" ^ string_of_expr e1 ^ ")"
| Int_Of(e1) -> "int_of(" ^ string_of_expr e1 ^ ")"
| Float_Of(e1) -> "float_of(" ^ string_of_expr e1 ^ ")"
| Floor(e1) -> "floor(" ^ string_of_expr e1 ^ ")"
| Ceil(e1) -> "ceil(" ^ string_of_expr e1 ^ ")"
| Round(e1) -> "round(" ^ string_of_expr e1 ^ ")"
| Abs(e1) -> "abs(" ^ string_of_expr e1 ^ ")"
| Log(e1) -> "log(" ^ string_of_expr e1 ^ ")"
| Inverse(e1) -> "inverse(" ^ string_of_expr e1 ^ ")"
| Solve(e1, e2) -> "solve(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
| Svd(e1) -> "svd(" ^ string_of_expr e1 ^ ")"
| Eig(e1) -> "eig(" ^ string_of_expr e1 ^ ")"
| Eigv(e1) -> "eigv(" ^ string_of_expr e1 ^ ")"
| FuncCall(e1, e2) -> (string_of_expr e1 ^ "(" ^ (String.concat "," (List.map string_of_expr e2))) ^ ")"

let rec string_of_stmt = function
  EmptyStmt -> ""
| Expr(e1) -> string_of_expr e1
(*| Assign(str1, e2) -> (str1 ^ " = " ^ string_of_expr e2)*)
(* | FuncSign(str1, str2) -> str1 ^ "(" ^ (String.concat "," str2) ^ ")" ^ "\n" *)
| FuncDecl(str1, str2, s1) -> "def " ^ str1 ^ "(" ^ String.concat "," str2 ^ ")" ^ "{\n" ^ String.concat ";\n" (List.map string_of_stmt s1) ^ "}\n"
| IfStmt(e1, s2, s3) -> (match s3 with
    | [EmptyStmt] -> "if (" ^ string_of_expr e1 ^ ")\n{\n" ^ String.concat "," (List.map string_of_stmt s2) ^ "}\n"
    | _ -> "if (" ^ string_of_expr e1 ^ ")\n{\n" ^ String.concat "," (List.map string_of_stmt s2) ^ "} else {" ^ String.concat "," (List.map string_of_stmt s3) ^ "\n}\n")
| ForStmt(str1, e1, s1) -> "for (" ^ str1 ^ " in " ^ string_of_expr e1 ^ ") {\n" ^ String.concat "," (List.map string_of_stmt s1) ^ "\n}\n"
| WhileStmt(e1, s1) -> "while (" ^ string_of_expr e1 ^ ") {\n" ^ String.concat "," (List.map string_of_stmt s1) ^ "\n}\n"
(* Parallel Environment *)
| PEInvoke(s1) -> "using " ^ s1
| PEEnd(s1) -> "end " ^ s1
| Return(e1) -> "return " ^ string_of_expr e1 ^ "\n"
| Break -> "break\n"
| Continue -> "continue\n"
| Exit(e1) -> "exit" ^ string_of_expr e1 ^ "\n"

let string_of_mapfunc (id, s) =
    "map " ^ id ^ "{\n" ^ String.concat "" (List.map string_of_stmt s) ^ "}\n"

let string_of_po po=
    "Overload " ^ po.operator ^
    "(" ^ String.concat "," po.params ^ ")" ^
    "{\n" ^
    String.concat "" (List.map string_of_stmt po.headstmt) ^
    String.concat "" (List.map string_of_mapfunc po.mapfuncs) ^
    "reduce " ^ "{\n" ^ String.concat "" (List.map string_of_stmt po.reducefunc) ^ "}\n" ^
    "}\n"

let string_of_pe (str1, p2) =
    "parallel_define " ^ str1 ^ "{\n" ^ String.concat "" (List.map string_of_po p2) ^ "}\n"

let string_of_program (pes,stmts) =
    String.concat "" (List.map string_of_pe pes) ^
    String.concat ";\n" (List.map string_of_stmt stmts) ^ "\n"
