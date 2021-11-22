(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

(* a hello world version of codegen *)

module L = Llvm
open Ast
open Sast 

module StringMap = Map.Make(String)

let gen_array ltype arr =
    let n = Array.length arr in
    let newarray = Array.make n 
      (match arr.(0) with
        IntLit(x) -> L.const_int ltype 0
      | FloatLit(x) -> L.const_float ltype 0.) in
    for i = 0 to (Array.length arr)-1 do
      newarray.(i) <-  
        (match arr.(i) with
          IntLit(x) -> L.const_int ltype x
        | FloatLit(x) -> L.const_float ltype x)
    done; L.const_array ltype newarray
  
let gen_dim ltype arr = 
let n = Array.length arr in
let newarray = Array.make n (L.const_int ltype 0) in
for i = 0 to (Array.length arr)-1 do
    newarray.(i) <-  L.const_int ltype arr.(i)
done; L.const_array ltype newarray

let gen_value ltype value = L.const_int ltype value

let set_constptr name lvalue lmodule ltype = 
let global_value = L.define_global name lvalue lmodule in
    L.set_global_constant true global_value; 
    L.set_linkage L.Linkage.Private global_value;
    L.set_unnamed_addr true global_value;
    L.const_bitcast global_value ltype
    
let translate sstmts =
  let gloabl_symbol_table = StringMap.empty in

  let context = L.global_context() in

  let the_module = L.create_module context "TENLab" in

  (* TODO: How to support tensor? Seems like it can be supported directly. *)
  let int_t     = L.i32_type      context
  and i8_t      = L.i8_type       context
  and float_t   = L.double_type   context
  and void_t    = L.void_type     context  in
  let i8ptr_t   = L.pointer_type  i8_t
  and tensor_t = L.named_struct_type context "tensor_t" in
        L.struct_set_body tensor_t [| i8_t; i8_t; i8ptr_t; i8ptr_t |] false;

  (* init *)
  let function_type = L.function_type i8_t [||] in
  let the_function = L.define_function "main" function_type the_module in

  (* built-in function *)
  let add_t : L.lltype = 
  L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let add_func : L.llvalue = 
  L.declare_function "add" add_t the_module in
  let mult_t : L.lltype = 
  L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let mult_func : L.llvalue = 
  L.declare_function "mult" mult_t the_module in
  let print_t : L.lltype = 
  L.function_type void_t [| i8ptr_t |] in
  let print_func : L.llvalue = 
  L.declare_function "print" print_t the_module in

  (* expression translation *)
  let rec genExpr builder se = match se with
      (_, SBinop(se1, op, se2)) -> 
        let se1_ = genExpr builder se1
        and se2_ = genExpr builder se2 in
        (match op with
          Add -> L.build_call add_func
        | Mul -> L.build_call mult_func
        ) [| se1_ ; se2_ |] "tmpOp" builder
    | (STensorTup(t, n, d), STensor(y)) ->
        (match t with 
          INT_Tensor -> set_constptr "stensor" (L.const_named_struct tensor_t 
            (let dims = set_constptr "sdim" (gen_dim i8_t d) the_module i8ptr_t
            and data = set_constptr "sdata" (gen_array int_t y) the_module i8ptr_t
            in [|gen_value i8_t 0; gen_value i8_t n; dims; data|])) the_module i8ptr_t
        | FLOAT_Tensor -> set_constptr "stensor" (L.const_named_struct tensor_t 
            (let dims = set_constptr "sdim" (gen_dim i8_t d) the_module i8ptr_t
            and data = set_constptr "sdata" (gen_array float_t y) the_module i8ptr_t
            in [|gen_value i8_t 1; gen_value i8_t n; dims; data|])) the_module i8ptr_t
        ) 
    | (_, _) -> gen_value i8ptr_t 0 in

  
  let rec stmt builder = function
      SExpr se -> ignore(genExpr builder se); builder in

  let builder = L.builder_at_end context (L.entry_block the_function) in
  let builder = List.fold_left stmt builder sstmts in
  ignore(L.build_ret (L.const_int i8_t 0) builder);
  the_module;