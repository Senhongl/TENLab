(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

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

let translate sstmts =
  let global_symbol_table = StringHash.create 10 in
  let global_function_table = StringHash.create 10 in

  let context = L.global_context() in

  let the_module = L.create_module context "TENLab" in

  let int_t     = L.i32_type      context
  and i8_t      = L.i8_type       context
  and bool_t    = L.i1_type       context
  and float_t   = L.double_type   context
  and i64_t     = L.i64_type      context
  and void_t    = L.void_type     context  in
  let i8ptr_t   = L.pointer_type  i8_t
  and tensor_t = L.named_struct_type context "tensor_t" 
  and int_array_t = L.array_type int_t  
  and float_array_t = L.array_type float_t in
        L.struct_set_body tensor_t [| i8_t; i8_t; i8ptr_t; i8ptr_t; i8_t |] false;

  let tensor_pt = L.pointer_type  tensor_t in
  let int_pt = L.pointer_type int_t in
  (* init *)
  let function_type = L.function_type i8_t [||] in
  let main_function = L.define_function "main" function_type the_module in

  (* set local pointer *)
  let set_localptr local_builder symbol_table args_name args_val =
    ignore(StringHash.remove symbol_table args_name);
    let alloca = L.build_alloca i8ptr_t args_name local_builder in
        ignore(L.build_store args_val alloca local_builder);
        StringHash.add symbol_table args_name alloca in


  (* create a new tensor *) 
  let build_tensor the_namespace ltype itype dtype ndims dims data =
    let size = Array.length data in
    let tensor = L.build_malloc tensor_t "raw_tensor" the_namespace.builder in
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
    ignore(List.iter2 store_dims (Array.to_list dims) (List.init size (fun i -> i)));

    (* store data *)
    let size = Array.length data in
    let dataptr = L.build_malloc (L.array_type ltype size) "data" the_namespace.builder in
    let dataptr_as_i8ptr = L.build_bitcast dataptr i8ptr_t "data_as_i8ptr" the_namespace.builder in
    
    let store_data ten idx =
      let gep_addr = L.build_gep dataptr [|L.const_int itype 0; L.const_int itype idx|] "elmptr" the_namespace.builder in
      ignore(L.build_store ten gep_addr the_namespace.builder);
    in
    ignore(List.iter2 store_data (Array.to_list data) (List.init size (fun i -> i)));

    let dimsptr_of_tensor = L.build_struct_gep tensor 2 "dimsptr" the_namespace.builder in
    ignore(L.build_store dimsptr_as_i8ptr dimsptr_of_tensor the_namespace.builder);

    let dataptr_of_tensor = L.build_struct_gep tensor 3 "dataptr" the_namespace.builder in
    ignore(L.build_store dataptr_as_i8ptr dataptr_of_tensor the_namespace.builder);

    (* init reference count *)
    let rcptr = L.build_struct_gep tensor 4 "rc" the_namespace.builder in
    ignore(L.build_store (L.const_int i8_t 0) rcptr the_namespace.builder);

    L.build_bitcast tensor i8ptr_t "tensor" the_namespace.builder
  in

  let get_tensor_length tensor the_namespace =
    let tensorptr = L.build_bitcast tensor tensor_pt "tensorptr" the_namespace.builder in
    let ndimsptr = L.build_struct_gep tensorptr 2 "ndimsptr" the_namespace.builder in
    let ndimsptr_as_intptr = L.build_bitcast ndimsptr int_pt "ndimsptr_as_intptr" the_namespace.builder in
    let length = L.build_load ndimsptr_as_intptr "length" the_namespace.builder in
    length
  in

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
  let index_get_t : L.lltype =
  L.function_type i8ptr_t [|i8ptr_t; i8ptr_t|] in
  let index_get : L.llvalue =
  L.declare_function "index_get" index_get_t the_module in
  let index_put_t : L.lltype =
  L.function_type void_t [|i8ptr_t; i8ptr_t; i8ptr_t|] in
  let index_put : L.llvalue =
  L.declare_function "index_put" index_put_t the_module in
  let increase_rc_t : L.lltype =
  L.function_type void_t [| i8ptr_t |] in
  let increase_rc: L.llvalue =
  L.declare_function "increase_rc" increase_rc_t the_module in
  let decrease_rc_t : L.lltype =
  L.function_type void_t [| i8ptr_t |] in
  let decrease_rc: L.llvalue =
  L.declare_function "decrease_rc" decrease_rc_t the_module in
  (* just for debug *)
  let print_int_t : L.lltype =
  L.function_type void_t [| int_t |] in
  let print_int : L.llvalue = 
  L.declare_function "print_int" print_int_t the_module in

  let lookup id the_namespace = if StringHash.mem the_namespace.symbol_table id then StringHash.find the_namespace.symbol_table id
                                else if the_namespace.global then let init = L.define_global id (L.const_pointer_null i8ptr_t) the_module in
                                                                  ignore(StringHash.add the_namespace.symbol_table id init);
                                                                  init
                                else let init = L.build_alloca i8ptr_t id the_namespace.builder in
                                     ignore(L.build_store (L.const_pointer_null i8ptr_t) init the_namespace.builder);
                                     ignore(StringHash.add the_namespace.symbol_table id init);
                                     init in

  (* expression translation *)
  let rec genExpr the_namespace se = match se with
    | (_, SBinop(se1, op, se2)) -> 
        let se1_ = genExpr the_namespace se1
        and se2_ = genExpr the_namespace se2 in
        (match op with
          Add -> L.build_call add_func
        | Mul -> L.build_call mult_func
        | Eq  -> L.build_call equal_func 
        ) [| se1_ ; se2_ |] "tmpOp" the_namespace.builder
        (* cast_voidpt_to_tensor the_namespace "tmpOp" tmpOp *)
    | (STensorTup(t, n, d), STensor(y)) ->
        (match t with 
          INT_Tensor -> build_tensor the_namespace int_t int_t (gen_value i8_t 0) (gen_value i8_t n) (gen_dim i8_t d) (gen_array int_t y)
        | FLOAT_Tensor -> build_tensor the_namespace float_t i64_t (gen_value i8_t 1) (gen_value i8_t n) (gen_dim i8_t d) (gen_array float_t y)
        )
    | (_, SVtensor(x)) -> let rec gen_vartensor = function
        Tensor0(x) -> (match x with
          IntLit(_) -> [|build_tensor the_namespace int_t int_t (gen_value i8_t 0) (gen_value i8_t 0) (gen_dim i8_t [||]) (gen_array int_t [|x|])|]
        | FloatLit(_) -> [|build_tensor the_namespace float_t i64_t (gen_value i8_t 1) (gen_value i8_t 0) (gen_dim i8_t [||]) (gen_array float_t [|x|])|])
      | LRTensor(x) -> let y = gen_vartensor(x) in
                       let dims = gen_dim i8_t [|Array.length(y)|] in
                       let data =  y in
                       let ptrx = build_tensor the_namespace i8ptr_t i64_t (gen_value i8_t 3) (gen_value i8_t 1) dims data in
                       [|ptrx|]
      | LRTensors(x1, x2) -> let y1 = gen_vartensor(x1) and y2 = gen_vartensor(x2) in
                             let dims = gen_dim i8_t [|Array.length(y1)+Array.length(y2)|] in
                             let data = Array.append y1 y2 in
                             let ptrx = build_tensor the_namespace i8ptr_t i64_t (gen_value i8_t 3) (gen_value i8_t 1) dims data in
                             [|ptrx|]
      | NPTensor(x) -> gen_vartensor(x)
      | NPTensors(x1, x2) -> let y1 = gen_vartensor(x1) and y2 = gen_vartensor(x2) in
                             Array.append y1 y2
      in let y = gen_vartensor(x) in y.(0)
    | (_, SASexpr(x)) -> (match x with 
        Id(id) -> L.build_load (lookup id the_namespace) id the_namespace.builder
      | Idind(s, x) -> (match x with
          (nlist, indlist) -> let rec gen_indlist = function
            [] -> [||]
          | (n, d, y)::indlist_ -> let i0 = genExpr the_namespace (STensorTup(INT_Tensor, n, d), STensor(y)) in 
            let y1 = [|i0|] and y2 = gen_indlist(indlist_) in Array.append y1 y2 in 
            let dims = gen_dim i8_t [|nlist|] in
            let data = gen_indlist(indlist) in
            let xptr = build_tensor the_namespace i8ptr_t i64_t (gen_value i8_t 3) (gen_value i8_t 1) dims data in
            let sptr = L.build_load (lookup s the_namespace) s the_namespace.builder in
            L.build_call index_get [|sptr; xptr|] "access_tensor" the_namespace.builder
          )
      )
    | (_, SPrint(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call print_func [| se1_ |] "" the_namespace.builder
    | (_, SFuncCall(str1, se1)) -> let (the_function, the_builder) = StringHash.find the_namespace.function_table str1 in
                                   let argv = List.map (genExpr the_namespace) se1 in
                                   L.build_call the_function (Array.of_list argv) "ret" the_namespace.builder
    | (_, _) -> gen_value i8ptr_t 0 in

  let build_fn fname argc =
    let ftype : L.lltype = 
      L.function_type i8ptr_t (Array.make argc i8ptr_t) in
    let the_function = L.define_function fname ftype the_module in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    (the_function, builder)
  in

  let rec stmt the_namespace = function
      SEmptyStmt -> the_namespace
    | SExpr(se) -> ignore(genExpr the_namespace se); the_namespace 
    | SAssign(s, se1) -> 
            (match s with 
              Id(id) ->
                          let rhs = genExpr the_namespace se1 in
                          let lhs = lookup id the_namespace in
                          let lhsptr = L.build_load lhs "lhsptr" the_namespace.builder in
                          ignore(L.build_call increase_rc [| rhs |] "" the_namespace.builder);
                          ignore(L.build_call decrease_rc [| lhsptr |] "" the_namespace.builder);
                          ignore(L.build_store rhs lhs the_namespace.builder);
                          the_namespace
            | Idind(s, x) ->
                          let rhs = genExpr the_namespace se1 in
                  (match x with
                          (nlist, indlist) -> let rec gen_indlist = function
                            [] -> [||]
                          | (n, d, y)::indlist_ -> let i0 = genExpr the_namespace (STensorTup(INT_Tensor, n, d), STensor(y)) in 
                            let y1 = [|i0|] and y2 = gen_indlist(indlist_) in Array.append y1 y2 in 
                            let dims = gen_dim i8_t [|nlist|] in
                            let data = gen_indlist(indlist) in
                            let xptr = build_tensor the_namespace i8ptr_t i64_t (gen_value i8_t 3) (gen_value i8_t 1) dims data in
                            let sptr = L.build_load (lookup s the_namespace) s the_namespace.builder in
                            L.build_call index_put [|sptr; xptr; rhs|] "" the_namespace.builder
                  ); the_namespace
            )
    | SFuncDecl(str1, str2, ss1) -> let argc = List.length(str2) in
                             let (the_function, the_builder) = build_fn str1 argc in
                             ignore(StringHash.add the_namespace.function_table str1 (the_function, the_builder));
                             let local_symbol_table = StringHash.copy the_namespace.symbol_table in
                             List.iter (fun id -> StringHash.remove local_symbol_table id) str2;
                             let local_function_table = StringHash.copy the_namespace.function_table in
                             let argv = Array.to_list (L.params the_function) in
                             List.iter2 (set_localptr the_builder local_symbol_table) str2 argv;
                             let build_return b = L.build_ret (L.const_pointer_null i8ptr_t) b in
                             let local_namespace = {symbol_table = local_symbol_table; function_table = local_function_table; func = the_function; builder = the_builder; global = false} in
                             let local_namespace = List.fold_left stmt local_namespace ss1 in
                             ignore(add_terminal the_builder build_return);
                             the_namespace (* return the main namespace *)
    | SIfStmt(se1, ss1, ss2) -> let se1_ = genExpr the_namespace se1 in
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
                              let bool_val = L.build_call bool_of_zero [| se1_ |] "bool" local_namespace.builder in
                              let merge_bb = L.append_block context "merge" the_namespace.func in
                              ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
                              let builder = L.builder_at_end context merge_bb in
                              {symbol_table = the_namespace.symbol_table; function_table = the_namespace.function_table; func = the_namespace.func; builder = builder; global = the_namespace.global}
    | SForStmt(str1, se1, ss1) -> let idxptr = L.build_alloca int_t "idxptr" the_namespace.builder in
                                  ignore(L.build_store (L.const_int int_t (0)) idxptr the_namespace.builder);
                                  let tensor = genExpr the_namespace se1 in
                                  let size = get_tensor_length tensor the_namespace in
                                  (* ignore(L.build_call print_int [| size |] "" the_namespace.builder); *)
                                  let pred_bb = L.append_block context "for" the_namespace.func in
                                  ignore(L.build_br pred_bb the_namespace.builder);
                                  let pred_builder = L.builder_at_end context pred_bb in
                                  let local_namespace = {symbol_table = the_namespace.symbol_table; 
                                                        function_table = the_namespace.function_table;
                                                        func = the_namespace.func; 
                                                        builder = pred_builder;
                                                        global = the_namespace.global} in
                                  let indicator = L.build_load idxptr "idx" pred_builder in
                                  let new_indicator = L.build_add indicator (L.const_int int_t 1) "new_idx"  pred_builder in
                                  ignore(L.build_store new_indicator idxptr pred_builder);
                                  (* ignore(L.build_call print_int [| indicator |] "" pred_builder); *)
                                  
                                  let cond = (L.build_icmp L.Icmp.Sgt) new_indicator size "condition" pred_builder in

                                  let body_bb = L.append_block context "for_body" the_namespace.func in
                                  let local_builder = L.builder_at_end context body_bb in
                                  let local_namespace = {symbol_table = the_namespace.symbol_table; 
                                                        function_table = the_namespace.function_table;
                                                        func = the_namespace.func; 
                                                        builder = local_builder;
                                                        global = the_namespace.global} in
                                  let local_namespace = List.fold_left stmt local_namespace ss1 in
                                  ignore(add_terminal local_namespace.builder (L.build_br pred_bb));

                                  let merge_bb = L.append_block context "merge" the_namespace.func in
                                  ignore(L.build_cond_br cond merge_bb body_bb pred_builder);
                                  let builder = L.builder_at_end context merge_bb in
                                  {symbol_table = the_namespace.symbol_table; function_table = the_namespace.function_table; func = the_namespace.func; builder = builder; global = the_namespace.global}
    | SReturn(se1) -> ignore(L.build_ret (genExpr the_namespace se1) the_namespace.builder); the_namespace
  in

  let main_builder = L.builder_at_end context (L.entry_block main_function) in
  let main_namespace = {symbol_table = global_symbol_table; function_table = global_function_table; func = main_function; builder = main_builder; global = true} in
  let main_namespace = List.fold_left stmt main_namespace sstmts in
  ignore(L.build_ret (L.const_int i8_t 0) main_namespace.builder);
  the_module;