(************ Abstract Syntax Tree types for TENLab ************)


type bop = 
(* Arithmetic operators*)  
Add | Sub | Mul | DotMul | Div | Pow | DotPow | Mod | FlrDiv

(* Relational operators *)
| Eq | Geq | Gt | Leq | Lt | Neq

(* Logical operators *)
| And | Or

(* Unary operators *)
type uop = Not | Neg | Transpose

(* Literal *)
(* TODO: support Tensor Literals *)
type literal = 
  IntLit of int
| FloatLit of float
| StringLit of string

(* TODO: support User-Defined Parallel function *)

(* TODO: support tensor! not just 0-dim data *)
(* TODO: support function call *)
type expr =
  Lit of literal
| Binop of expr * bop * expr
| Unop of uop * expr
| Range of expr * expr * expr
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

(* Statements *)
type stmt = 
  EmptyStmt
| Expr of expr
| FuncSign of string * string list 
| FuncDecl of stmt * stmt list
| FuncCall of string list * stmt
| Tdecl of string list * stmt
| Return of string list
| Break
| Continue
| Exit
| IfStmt of stmt * stmt list * stmt list
| ForStmt of string * expr * stmt list
| WhileStmt of expr * stmt list

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
| StringLit(l) -> l

let rec string_of_expr = function
  Lit(l) -> string_of_lit l
| Binop(e1, bop, e2) -> string_of_expr e1 ^ " " ^ string_of_bop bop ^ " " ^ string_of_expr e2
| Unop(uop, e1) -> string_of_uop uop ^ " " ^ string_of_expr e1
| Range(e1, e2, e3) -> string_of_expr e1 ^ ":" ^ string_of_expr e2 ^ ":" ^ string_of_expr e3
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
| Log(e1) -> "log (" ^ string_of_expr e1 ^ ")"
| Inverse(e1) -> "inverse (" ^ string_of_expr e1 ^ ")"
| Solve(e1, e2) -> "solve (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
| Svd(e1) -> "svd (" ^ string_of_expr e1 ^ ")"
| Eig(e1) -> "eig (" ^ string_of_expr e1 ^ ")"
| Eigv(e1) -> "Eigv (" ^ string_of_expr e1 ^ ")"

let concat l = List.fold_left (fun a b -> a ^ ", " ^ b) (List.hd l) (List.tl l)

let rec string_of_stmt = function
  EmptyStmt -> ""
| Expr(e1) -> string_of_expr e1
| FuncSign(str1, str2) -> str1 ^ "(" ^ concat str2 ^ ")" ^ "\n"
| FuncDecl(s1, s2) -> "def " ^ string_of_stmt s1 ^ "{\n" ^ concat (List.map string_of_stmt s2) ^ "\n}\n"
| FuncCall(str1, s1) ->  concat str1 ^ " = " ^ string_of_stmt s1
| Tdecl(str1, s1) -> concat str1 ^ " = " ^ string_of_stmt s1 ^ "\n"
| Return(str1) -> "return " ^ concat str1 ^ "\n"
| Break -> "break\n"
| Continue -> "continue\n"
| Exit -> "exit\n"
| IfStmt(s1, s2, s3) -> (match s3 with
    | [EmptyStmt] -> "if (" ^ string_of_stmt s1 ^ ")\n{\n" ^ concat (List.map string_of_stmt s2) ^ "}\n"
    | _ -> "if (" ^ string_of_stmt s1 ^ ")\n{\n" ^ concat (List.map string_of_stmt s2) ^ "} else {" ^ concat (List.map string_of_stmt s3) ^ "\n}\n")
| ForStmt(str1, e1, s1) -> "for (" ^ str1 ^ " in " ^ string_of_expr e1 ^ ") {\n" ^ concat (List.map string_of_stmt s1) ^ "\n}\n"
| WhileStmt(e1, s1) -> "while (" ^ string_of_expr e1 ^ ") {\n" ^ concat (List.map string_of_stmt s1) ^ "\n}\n"


and string_of_program l = String.concat "" (List.map string_of_stmt l) ^ "\n\n"