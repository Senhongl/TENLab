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

module StringHash = Hashtbl.Make(struct
  type t = string
  let equal x y = x = y
  let hash = Hashtbl.hash
end)

exception E of string (* debug information *)

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
  let global_symbol_table = StringHash.create 10 in
  let function_table = StringHash.create 10 in

  let context = L.global_context() in

  let the_module = L.create_module context "TENLab" in

  let int_t     = L.i32_type      context
  and i8_t      = L.i8_type       context
  and float_t   = L.double_type   context
  and void_t    = L.void_type     context  in
  let i8ptr_t   = L.pointer_type  i8_t
  and tensor_t = L.named_struct_type context "tensor_t" in
        L.struct_set_body tensor_t [| i8_t; i8_t; i8ptr_t; i8ptr_t |] false;

  let tensor_pt = L.pointer_type  tensor_t in

  (* init *)
  let function_type = L.function_type i8_t [||] in
  let the_function = L.define_function "main" function_type the_module in

  (* set local pointer *)
  let set_localptr local_builder symbol_table args_name args_val =
    L.set_value_name args_name args_val;
    let alloca = L.build_alloca i8ptr_t args_name local_builder in
        (* let args_val = L.const_bitcast args_val tensor_pt in *)
        ignore(L.build_store args_val alloca local_builder);
        let ptr = L.build_load alloca "local" local_builder in
        let ptr = L.const_bitcast ptr i8ptr_t in
        StringHash.add symbol_table args_name ptr in

  (* add terminal for function definition *)
  let add_terminal builder instr = 
      match L.block_terminator (L.insertion_block builder) with  
	    Some _ -> ()   (* do nothing if terminator is there *)
      | None -> ignore (instr builder) in

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

  let lookup id symbol_table = try StringHash.find symbol_table id
                               with Not_found -> raise (E "bug!!!!!!!") in

  (* expression translation *)
  let rec genExpr symbol_table builder se = match se with
      (* (_, SFId(id)) -> what's llvalue of sfid?  *)
    | (_, SId(id)) -> lookup id symbol_table
    | (_, SBinop(se1, op, se2)) -> 
        let se1_ = genExpr symbol_table builder se1
        and se2_ = genExpr symbol_table builder se2 in
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
    | (_, SPrint(se1)) -> let se1_ = genExpr symbol_table builder se1 in
                          L.build_call print_func [| se1_ |] "" builder
    | (_, SFuncCall(str1, se1)) -> let (the_function, the_builder) = StringHash.find function_table str1 in
                                   (* let se_to_args se = 
                                    let alloca = L.build_alloca i8ptr_t "arg" builder in
                                    let tensor = genExpr builder se in
                                    L.build_store tensor alloca builder in
                                   let argv = List.map se_to_args se1 in *)
                                   let argv = List.map (genExpr symbol_table the_builder) se1 in
                                   L.build_call the_function (Array.of_list argv) "ret" builder
    | (_, _) -> gen_value i8ptr_t 0 in

  let build_fn fname argc =
    let ftype : L.lltype = 
      L.function_type i8ptr_t (Array.make argc i8ptr_t) in
    let the_function = L.define_function fname ftype the_module in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    (the_function, builder)
  in

  let rec stmt symbol_table builder = function
      SExpr(se) -> ignore(genExpr symbol_table builder se); builder 
    | SAssign(str1, se1) -> let llvalue = genExpr symbol_table builder se1 in
                            ignore(StringHash.add symbol_table str1 llvalue); builder
    | SFuncDecl(str1, str2, ss1) -> let argc = List.length(str2) in
                             let (the_function, the_builder) = build_fn str1 argc in
                             ignore(StringHash.add function_table str1 (the_function, the_builder));
                             let local_symbol_table = StringHash.copy symbol_table in
                             let argv = Array.to_list (L.params the_function) in
                             List.iter2 (set_localptr the_builder local_symbol_table) str2 argv;
                             let build_return b = L.build_ret (L.const_int i8_t 0) b in (* TODO: return 0 if no return statement, it's a bug!!!!!!! *)
                             let the_builder = List.fold_left (stmt local_symbol_table) the_builder ss1 in
                             ignore(add_terminal the_builder build_return); 
                             builder (* return the main builder *)
    | SReturn(se1) -> ignore(L.build_ret (genExpr symbol_table builder se1) builder); builder
  in

  let builder = L.builder_at_end context (L.entry_block the_function) in
  let builder = List.fold_left (stmt global_symbol_table) builder sstmts in
  ignore(L.build_ret (L.const_int i8_t 0) builder);
  the_module;