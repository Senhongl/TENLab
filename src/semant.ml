(* Semantic checking for the TENLab compiler *)

open Ast
open Sast

module StringHash = Hashtbl.Make(struct
  type t = string
  let equal x y = x = y
  let hash = Hashtbl.hash
end)

let symbol_table = StringHash.create 10
let function_table = StringHash.create 10
let pe_table = StringHash.create 10

let rec equal_dim d1 d2=
  match d1, d2 with
    d10::d1_, d20::d2_ -> if d10 <> d20 then false else equal_dim d1_ d2_
  | [], [] -> true
  | _, [] -> false
  | [], _ -> false

let rec check_tensor = function
  Tensor0(x) -> (match x with
    IntLit(y) -> (TensorTup(INT_Tensor, 0, [-1]), [|x|])
  | FloatLit(y) -> (TensorTup(FLOAT_Tensor, 0, [-1]), [|x|]))
| LRTensor(x) -> 
    (match check_tensor(x) with
      (TensorTup(t, nd, d0::d_), y) -> (TensorTup(t, nd+1, -1::-d0::d_), y)
    | _ -> raise (Failure("ought not occur")))
| NPTensor(x) -> check_tensor(x)
| LRTensors(x1, x2) -> 
    let tdy1 = check_tensor(x1) in
    let tdy2 = check_tensor(x2) in
      (match tdy1, tdy2 with
        (TensorTup(t1, nd1, d10::d1_), y1), (TensorTup(t2, nd2, d20::d2_), y2) -> 
          if t1 = t2 && equal_dim d1_ d2_ then 
            (TensorTup(t1, nd1+1, -1::-(d10 + d20)::d1_), Array.append y1 y2)
          else if t1 <> t2 then raise (Failure("invalid type"))
          else raise (Failure("invalid dim"))
      | _, _ -> raise (Failure("ought not occur")))
| NPTensors(x1, x2) -> 
    let tdy1 = check_tensor(x1) in
    let tdy2 = check_tensor(x2) in
      (match tdy1, tdy2 with
        (TensorTup(t1, nd1, d10::d1_), y1), (TensorTup(t2, nd2, d20::d2_), y2) -> 
          if t1 = t2 && equal_dim d1_ d2_ then 
            (TensorTup(t1, nd1, (d10 + d20)::d1_), Array.append y1 y2)
          else if t1 <> t2 then raise (Failure("invalid type"))
          else raise (Failure("invalid dim"))
      | _, _ -> raise (Failure("ought not occur")))

(*let check_type = function
  INT_Tensor -> 0
| FLOAT_Tensor -> 1

let rec check_intv = function
  LRTensor(x) -> check_intv(NPTensor(x))
| NPTensor(x) -> (match check_tensor(x) with 
  | (TensorTup(t, n, d::d_), y) -> if check_type(t) = 0 then
    (1, [(n, Array.of_list d_, y)]) else raise (Failure("Index must be integer tensor"))
  | (_, _) -> raise (Failure("ought not occur")))
| LRTensors(x1, x2) -> check_intv(NPTensors(x1, x2))
| NPTensors(x1, x2) -> let y1 = check_intv(NPTensor(x1)) and y2 = check_intv(x2) in
    (match y1, y2 with
      (n1, ind1), (n2, ind2) -> (n1+n2, ind1@ind2))
| _ -> raise (Failure("wrong format of index"))*)

(* expr -> sexpr *)
let rec check_expr symbol_table function_table = function
| FId(id) -> if StringHash.mem symbol_table id then StringHash.remove symbol_table id;
             if StringHash.mem function_table id then (SVoidTup, SFId(id)) else raise (Failure("function " ^ id ^ " not defined"))
| Binop(x1, bop, x2) -> (SVoidTup, SBinop(check_expr symbol_table function_table x1, bop, check_expr symbol_table function_table x2))
| Unop(uop, x) -> (SVoidTup, SUnop(uop, check_expr symbol_table function_table x))
| Range(x1, x2, x3) -> 
  let x1_ = check_expr symbol_table function_table x1 in
  let x2_ = check_expr symbol_table function_table x2 in 
  let x3_ = check_expr symbol_table function_table x3 in
    (SVoidTup, SRange(x1_, x2_, x3_))
| Tensor(x) -> (match check_tensor(x) with 
  | (TensorTup(t, n, d::d_), y) -> (STensorTup(t, n, Array.of_list d_), STensor(y))
  | (_, _) -> raise (Failure( "ought not occur")))
| VarTs(x) -> 
      let x_ = List.map (check_expr symbol_table function_table) x in
              (SVoidTup, SVtensor(x_))
| ASexpr(x) -> (match x with 
  | Id(id) -> if StringHash.mem function_table id then StringHash.remove function_table id;
              if StringHash.mem symbol_table id then (SVoidTup, SASexpr(Id(id)))
              else raise (Failure( "variable " ^ id ^ " not defined"))
  | Idind(id, x) -> 
      let x_ = List.map (check_expr symbol_table function_table) x in
                    if StringHash.mem function_table id then StringHash.remove function_table id;
                    if StringHash.mem symbol_table id then (SVoidTup, SASexpr(Idind(id, x_)))
                    else raise (Failure( "variable " ^ id ^ " not defined")))
| Print(e) -> (SVoidTup, SPrint(check_expr symbol_table function_table e))
| FuncCall(e1, e2) -> let e1_ = check_expr symbol_table function_table e1 in
                      let e2_ = List.map (check_expr symbol_table function_table) e2 in
                      let FId(id) = e1 in
                      let argc = StringHash.find function_table id in
                      if argc <> List.length(e2) then raise (Failure("the number of arguments mismatch"))
                      else (SVoidTup, SFuncCall(id, e2_))

(* stmt -> sstmt *)
let rec check_stmt symbol_table function_table = function
  EmptyStmt -> SEmptyStmt
| Expr(e) -> SExpr(check_expr symbol_table function_table e)
| Assign(s, e2) -> 
                      let sexpr = check_expr symbol_table function_table e2 in
      (match s with
            Id(str1) ->
                      ignore(StringHash.add symbol_table str1 sexpr); SAssign(Id(str1), sexpr)
          | Idind(id, x) -> 
                let x_ = List.map (check_expr symbol_table function_table) x in
                      if StringHash.mem function_table id then StringHash.remove function_table id;
                      if StringHash.mem symbol_table id then SAssign(Idind(id, x_), sexpr)
                      else raise (Failure( "variable " ^ id ^ " not defined"))
      )
| IfStmt(e1, s1, s2) -> let local_symbol_table = StringHash.copy symbol_table in
                        let local_function_table = StringHash.copy function_table in
                        let e1_ = check_expr local_symbol_table local_function_table e1 in (* TODO: check if boolean expression *)
                        let s1_ = List.map (check_stmt local_symbol_table local_function_table) s1 in
                        let s2_ = List.map (check_stmt local_symbol_table local_function_table) s2 in
                        SIfStmt(e1_, s1_, s2_)
| WhileStmt(e1, s1) -> let local_symbol_table = StringHash.copy symbol_table in
                       let local_function_table = StringHash.copy function_table in
                       let e1_ = check_expr local_symbol_table local_function_table e1 in
                       let s1_ = List.map (check_stmt local_symbol_table local_function_table) s1 in
                       SWhileStmt(e1_, s1_)
| ForStmt(str1, e1, s1) -> let local_symbol_table = StringHash.copy symbol_table in
                           ignore(StringHash.add local_symbol_table str1 (SVoidTup, SVoidExpr));
                           let local_function_table = StringHash.copy function_table in
                           let e1_ = check_expr local_symbol_table local_function_table e1 in
                           let s1_ = List.map (check_stmt local_symbol_table local_function_table) s1 in
                           SForStmt(str1, e1_, s1_)
| FuncDecl(str1, str2, s1) -> let local_symbol_table = StringHash.copy symbol_table in
                              let argc = List.length(str2) in
                              List.iter (fun s -> StringHash.add local_symbol_table s (SVoidTup, SVoidExpr)) str2;
                              ignore(StringHash.add function_table str1 argc);
                              let local_function_table = StringHash.copy function_table in
                              let s1_ = List.map(check_stmt local_symbol_table local_function_table) s1 in
                              SFuncDecl(str1, str2, s1_)
| Return(e1) -> let e1_ = check_expr symbol_table function_table e1 in
                SReturn(e1_)
| Break -> SBreak
| Continue -> SContinue
| Exit(e1) -> let e1_ = check_expr symbol_table function_table e1 in
              SExit(e1_)
| PEInvoke(s1) -> let z = StringHash.find pe_table s1 in
if (z=1) then SPEInvoke(s1) else raise (Failure("PE does not exist"))
| PEEnd(s1) -> let z = StringHash.find pe_table s1 in
if (z=1) then  SPEEnd(s1)  else raise (Failure("PE does not exist"))

let check_mapf s f (name,statements) = 
let ps = StringHash.copy s in
let fs = StringHash.copy f in
(name, List.map (check_stmt ps fs) statements)

let check_po po =
let psymbol_table = StringHash.create 10 in
let pfunction_table = StringHash.create 10 in
ignore(List.iter (fun s -> StringHash.add psymbol_table s (SVoidTup, SVoidExpr)) po.params);
let app x =
ignore(List.iter (fun (name,_) -> StringHash.add psymbol_table name (SVoidTup, SVoidExpr)) po.mapfuncs); x
in
let oprewrite = function
  | "__+__" -> "ADD"
  | "__-__" -> "SUB"
  | "__*__" -> "MUL"
in 
{
  soperator = oprewrite po.operator;
  sparams = po.params; 
  smapfuncs = List.map (check_mapf psymbol_table pfunction_table) po.mapfuncs;
  sreducefunc = List.map (check_stmt psymbol_table pfunction_table) (app po.reducefunc)
}

let fill_pe pe spo =
match spo.soperator with 
| "ADD" -> {sadd = SPO(spo);
            sminus = pe.sminus;
            smulti = pe.smulti;}
| "SUB" -> {sadd = pe.sadd;
            sminus = SPO(spo);
            smulti = pe.smulti;}
| "MUL" -> {sadd = pe.sadd;
            sminus = pe.sminus;
            smulti = SPO(spo);}

let check_pe (name, pos) =
ignore(StringHash.add pe_table name 1);
(name, List.fold_left fill_pe {sadd = SDEF; sminus = SDEF; smulti = SDEF;} (List.map check_po pos))

let check (pes,stmts) = 
let z1 = List.map check_pe pes in
let z2 = List.map (check_stmt symbol_table function_table) stmts in
(z1, z2)