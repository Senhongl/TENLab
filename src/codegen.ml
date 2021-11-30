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

type namespace = {
    symbol_table: L.llvalue StringHash.t;
    function_table: (L.llvalue * L.llbuilder) StringHash.t;
    func: L.llvalue;
    builder: L.llbuilder;
    global: bool
}

exception E of string (* debug information *)

let seq len =
  let rec aux len acc =
    if len < 0 then acc else aux (len - 1) (len::acc)
  in aux (len - 1) []

(* let gen_array ltype arr =
    let n = Array.length arr in
    let newarray = Array.make n 
      (match ltype with
        int_t -> L.const_int ltype 0
      | float_t -> L.const_float ltype 0.) in
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
  done; L.const_array ltype newarray *)

let gen_array ltype arr =
    let n = Array.length arr in
    let newarray = Array.make n 
      (match ltype with
        int_t -> L.const_int ltype 0
      | float_t -> L.const_float ltype 0.) in
    for i = 0 to (Array.length arr)-1 do
      newarray.(i) <-  
        (match arr.(i) with
          IntLit(x) -> L.const_int ltype x
        | FloatLit(x) -> L.const_float ltype x)
    done; newarray
  
let gen_dim ltype arr = 
  let n = Array.length arr in
  let newarray = Array.make n (L.const_int ltype 0) in
  for i = 0 to (Array.length arr)-1 do
      newarray.(i) <-  L.const_int ltype arr.(i)
  done; newarray

let gen_value ltype value = L.const_int ltype value

(* let set_constptr name lvalue lmodule ltype = 
  let global_value = L.define_global name lvalue lmodule in
      L.set_global_constant true global_value; 
      L.set_linkage L.Linkage.Private global_value;
      L.set_unnamed_addr true global_value;
      L.const_bitcast global_value ltype *)

let translate sstmts =
  let global_symbol_table = StringHash.create 10 in
  let global_function_table = StringHash.create 10 in

  let context = L.global_context() in

  let the_module = L.create_module context "TENLab" in

  let int_t     = L.i32_type      context
  and i8_t      = L.i8_type       context
  and bool_t    = L.i1_type       context
  and float_t   = L.double_type   context
  and void_t    = L.void_type     context  in
  let i8ptr_t   = L.pointer_type  i8_t
  and tensor_t = L.named_struct_type context "tensor_t" 
  and int_array_t = L.array_type int_t  
  and float_array_t = L.array_type float_t in
        L.struct_set_body tensor_t [| i8_t; i8_t; i8ptr_t; i8ptr_t |] false;

  let tensor_pt = L.pointer_type  tensor_t in

  (* init *)
  let function_type = L.function_type i8_t [||] in
  let main_function = L.define_function "main" function_type the_module in

  (* set local pointer *)
  let set_localptr local_builder symbol_table args_name args_val =
    L.set_value_name args_name args_val;
    let alloca = L.build_alloca tensor_t args_name local_builder in
        (* let args_val = L.const_bitcast args_val tensor_pt in *)
        ignore(L.build_store args_val alloca local_builder);
        (* let ptr = L.build_load alloca "local" local_builder in *)
        (* let ptr = L.const_bitcast ptr tensor_t in *)
        StringHash.add symbol_table args_name alloca in

  (* create a new tensor *)
  let build_tensor the_namespace ltype dtype ndims dims data =
    let size = Array.length data in
    let tensor = L.build_malloc tensor_t "tensor" the_namespace.builder in
    (* store dtype *)
    let dtypeptr = L.build_struct_gep tensor 0 "dtype" the_namespace.builder in
    ignore(L.build_store dtype dtypeptr the_namespace.builder);

    (* store ndims *)
    let ndimsptr = L.build_struct_gep tensor 1 "ndims" the_namespace.builder in
    ignore(L.build_store ndims ndimsptr the_namespace.builder);

    (* store dims *)
    let size = Array.length dims in
    let dimsptr = L.build_malloc (L.array_type i8_t size) "dims" the_namespace.builder in
    let dimsptr_as_i8ptr = L.build_bitcast dimsptr i8ptr_t "dims_as_i8ptr" the_namespace.builder in
    let store_dims dim idx = 
      let gep_addr = L.build_gep dimsptr [|L.const_int i8_t 0; L.const_int i8_t idx|] "elmptr" the_namespace.builder in
      ignore(L.build_store dim gep_addr the_namespace.builder);
    in
    ignore(List.iter2 store_dims (Array.to_list dims) (seq size));

    (* store data *)
    let size = Array.length data in
    let dataptr = L.build_malloc (L.array_type ltype size) "data" the_namespace.builder in
    let dataptr_as_i8ptr = L.build_bitcast dataptr i8ptr_t "data_as_i8ptr" the_namespace.builder in
    let store_data ten idx = 
      let gep_addr = L.build_gep dataptr [|L.const_int ltype 0; L.const_int ltype idx|] "elmptr" the_namespace.builder in
      ignore(L.build_store ten gep_addr the_namespace.builder);
    in
    ignore(List.iter2 store_data (Array.to_list data) (seq size));
    

    let dimsptr_of_tensor = L.build_struct_gep tensor 2 "dimsptr" the_namespace.builder in
    ignore(L.build_store dimsptr_as_i8ptr dimsptr_of_tensor the_namespace.builder);

    let dataptr_of_tensor = L.build_struct_gep tensor 3 "dataptr" the_namespace.builder in
    ignore(L.build_store dataptr_as_i8ptr dataptr_of_tensor the_namespace.builder);

    L.build_load tensor "tensor" the_namespace.builder
  in

  (* cast tensor to void pointer *)
  let cast_tensor_to_voidpt the_namespace name tensor =
    let alloca = L.build_alloca tensor_t name the_namespace.builder in
    ignore(L.build_store tensor alloca the_namespace.builder);
    L.build_bitcast alloca i8ptr_t name the_namespace.builder in

  (* cast void pointer back to tensor *)
  let cast_voidpt_to_tensor the_namespace name voidpt =
    let cast_val = L.build_bitcast voidpt tensor_pt name the_namespace.builder in
    L.build_load cast_val name the_namespace.builder in
  
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
  let equal_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let equal_func : L.llvalue = 
    L.declare_function "equal" equal_t the_module in
  let print_t : L.lltype = 
  L.function_type void_t [| i8ptr_t |] in
  let print_func : L.llvalue = 
  L.declare_function "print" print_t the_module in
  let bool_of_zero_t : L.lltype =
  L.function_type bool_t [| i8ptr_t |] in
  let bool_of_zero : L.llvalue = 
  L.declare_function "bool_of_zero" bool_of_zero_t the_module in
 
  let lookup id the_namespace = if StringHash.mem the_namespace.symbol_table id then StringHash.find the_namespace.symbol_table id
                                else if the_namespace.global then let init = L.define_global id (L.const_null tensor_t) the_module in
                                                                  ignore(StringHash.add the_namespace.symbol_table id init);
                                                                  init
                                else let init = L.build_alloca tensor_t id the_namespace.builder in
                                     ignore(StringHash.add the_namespace.symbol_table id init);
                                     init in

  (* expression translation *)
  let rec genExpr the_namespace se = match se with
      (* (_, SFId(id)) -> what's llvalue of sfid?  *)
    | (_, SId(id)) -> L.build_load (lookup id the_namespace) id the_namespace.builder
    | (_, SBinop(se1, op, se2)) -> 
        let se1_ = genExpr the_namespace se1
        and se2_ = genExpr the_namespace se2 in
        let se1_ = cast_tensor_to_voidpt the_namespace "arg1" se1_
        and se2_ = cast_tensor_to_voidpt the_namespace "arg2" se2_ in
        let tmpOp = (match op with
          Add -> L.build_call add_func
        | Mul -> L.build_call mult_func
        | Eq  -> L.build_call equal_func 
        ) [| se1_ ; se2_ |] "tmpOp" the_namespace.builder in
        cast_voidpt_to_tensor the_namespace "tmpOp" tmpOp
    | (STensorTup(t, n, d), STensor(y)) ->
        (* (match t with 
          INT_Tensor -> set_constptr "stensor" (L.const_named_struct tensor_t 
            (let dims = set_constptr "sdim" (gen_dim i8_t d) the_module i8ptr_t
            and data = set_constptr "sdata" (gen_array int_t y) the_module i8ptr_t
            in [|gen_value i8_t 0; gen_value i8_t n; dims; data|])) the_module tensor_pt
        | FLOAT_Tensor -> set_constptr "stensor" (L.const_named_struct tensor_t 
            (let dims = set_constptr "sdim" (gen_dim i8_t d) the_module i8ptr_t
            and data = set_constptr "sdata" (gen_array float_t y) the_module i8ptr_t
            in [|gen_value i8_t 1; gen_value i8_t n; dims; data|])) the_module tensor_pt
        )  *)
        (match t with 
          INT_Tensor -> build_tensor the_namespace int_t (gen_value i8_t 0) (gen_value i8_t n) (gen_dim i8_t d) (gen_array int_t y)
        | FLOAT_Tensor -> build_tensor the_namespace float_t (gen_value i8_t 1) (gen_value i8_t n) (gen_dim i8_t d) (gen_array float_t y)
        )
    | (_, SPrint(se1)) -> let se1_ = genExpr the_namespace se1 in
                          let tmp = cast_tensor_to_voidpt the_namespace "print_arg" se1_ in
                          L.build_call print_func [| tmp |] "" the_namespace.builder
    | (_, SFuncCall(str1, se1)) -> let (the_function, the_builder) = StringHash.find the_namespace.function_table str1 in
                                   let argv = List.map (genExpr the_namespace) se1 in
                                   (* let argv = List.map (cast_tensor_to_voidpt the_namespace "argv") tensor_argv in *)
                                   L.build_call the_function (Array.of_list argv) "ret" the_namespace.builder
    | (_, _) -> gen_value i8ptr_t 0 in

  let build_fn fname argc =
    let ftype : L.lltype = 
      L.function_type tensor_t (Array.make argc tensor_t) in
    let the_function = L.define_function fname ftype the_module in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    (the_function, builder)
  in

  let rec stmt the_namespace = function
      SEmptyStmt -> the_namespace
    | SExpr(se) -> ignore(genExpr the_namespace se); the_namespace 
    | SAssign(id, se1) -> let rhs = genExpr the_namespace se1 in
                          let lhs = lookup id the_namespace in
                          ignore(L.build_store rhs lhs the_namespace.builder);
                          (* if StringHash.mem the_namespace.symbol_table id then StringHash.remove the_namespace.symbol_table id;
                          StringHash.add the_namespace.symbol_table id rhs; *)
                          the_namespace
    | SFuncDecl(str1, str2, ss1) -> let argc = List.length(str2) in
                             let (the_function, the_builder) = build_fn str1 argc in
                             ignore(StringHash.add the_namespace.function_table str1 (the_function, the_builder));
                             let local_symbol_table = StringHash.copy the_namespace.symbol_table in
                             let local_function_table = StringHash.copy the_namespace.function_table in
                             let argv = Array.to_list (L.params the_function) in
                             List.iter2 (set_localptr the_builder local_symbol_table) str2 argv;
                             let build_return b = L.build_ret (L.const_null tensor_t) b in
                             let local_namespace = {symbol_table = local_symbol_table; function_table = local_function_table; func = the_function; builder = the_builder; global = false} in
                             let local_namespace = List.fold_left stmt local_namespace ss1 in
                             ignore(add_terminal the_builder build_return); 
                             the_namespace (* return the main namespace *)
    | SIfStmt(se1, ss1, ss2) -> let se1_ = genExpr the_namespace se1 in
                                let se1_ = cast_tensor_to_voidpt the_namespace "bool_of_zero" se1_ in
                                let bool_val = L.build_call bool_of_zero [| se1_ |] "bool" the_namespace.builder in
                                let merge_bb = L.append_block context "merge" the_namespace.func in
                                let build_br_merge = L.build_br merge_bb in
                                let then_bb = L.append_block context "then" the_namespace.func in
                                let local_builder = L.builder_at_end context then_bb in
                                let local_namespace = {symbol_table = the_namespace.symbol_table; 
                                                       function_table = the_namespace.function_table;
                                                       func = the_namespace.func; 
                                                       builder = local_builder;
                                                       global = the_namespace.global} in
                                let local_namespace = List.fold_left stmt local_namespace ss1 in
                                ignore(add_terminal local_namespace.builder build_br_merge);
                                let else_bb = L.append_block context "else" the_namespace.func in
                                let local_builder = L.builder_at_end context else_bb in
                                let local_namespace = {symbol_table = the_namespace.symbol_table; 
                                                       function_table = the_namespace.function_table;
                                                       func = the_namespace.func; 
                                                       builder = local_builder;
                                                       global = the_namespace.global} in
                                let local_namespace = List.fold_left stmt local_namespace ss2 in
                                ignore(add_terminal local_namespace.builder build_br_merge);
                                ignore(L.build_cond_br bool_val then_bb else_bb the_namespace.builder);
                                let builder = L.builder_at_end context merge_bb in
                                {symbol_table = the_namespace.symbol_table; function_table = the_namespace.function_table; func = the_namespace.func; builder = builder; global = the_namespace.global}
    | SWhileStmt(se1, ss1) -> let pred_bb = L.append_block context "while" the_namespace.func in
                              ignore(L.build_br pred_bb the_namespace.builder);
                              let body_bb = L.append_block context "while_body" the_namespace.func in
                              let local_builder = L.builder_at_end context body_bb in
                              let local_namespace = {symbol_table = the_namespace.symbol_table; 
                                                     function_table = the_namespace.function_table;
                                                     func = the_namespace.func; 
                                                     builder = local_builder;
                                                     global = the_namespace.global} in
                              let local_namespace = List.fold_left stmt local_namespace ss1 in
                              ignore(add_terminal local_namespace.builder (L.build_br pred_bb));
                              let pred_builder = L.builder_at_end context pred_bb in
                              let local_namespace = {symbol_table = the_namespace.symbol_table;
                                                     function_table = the_namespace.function_table;
                                                     func = the_namespace.func;
                                                     builder = pred_builder;
                                                     global = the_namespace.global} in
                              let se1_ = genExpr local_namespace se1 in
                              let se1_ = cast_tensor_to_voidpt local_namespace "bool_of_zero" se1_ in
                              let bool_val = L.build_call bool_of_zero [| se1_ |] "bool" local_namespace.builder in
                              let merge_bb = L.append_block context "merge" the_namespace.func in
                              ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
                              let builder = L.builder_at_end context merge_bb in
                              {symbol_table = the_namespace.symbol_table; function_table = the_namespace.function_table; func = the_namespace.func; builder = builder; global = the_namespace.global}
    | SReturn(se1) -> ignore(L.build_ret (genExpr the_namespace se1) the_namespace.builder); the_namespace
  in

  let main_builder = L.builder_at_end context (L.entry_block main_function) in
  let main_namespace = {symbol_table = global_symbol_table; function_table = global_function_table; func = main_function; builder = main_builder; global = true} in
  let main_namespace = List.fold_left stmt main_namespace sstmts in
  ignore(L.build_ret (L.const_int i8_t 0) main_namespace.builder);
  the_module;