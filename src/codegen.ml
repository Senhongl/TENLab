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
    global: bool;
    env : string;
}

type po = {
  funcnum : int;
  mapfuncs : L.llvalue;
  refunc : L.llvalue;
}

type actualpo = DEF | PO of po


type pe = {
  add : actualpo;
  minus : actualpo;
  multi : actualpo;
}

exception E of string (* debug information *)

let translate (spes,sstmts) =
  let global_symbol_table = StringHash.create 10 in
  let global_function_table = StringHash.create 10 in
  let global_pe_table = StringHash.create 10 in

  let context = L.global_context() in

  let the_module = L.create_module context "TENLab" in

  let int_t     = L.i32_type      context
  and i8_t      = L.i8_type       context
  and bool_t    = L.i1_type       context
  and float_t   = L.double_type   context
  and i64_t     = L.i64_type      context
  and void_t    = L.void_type     context  in
  let i8ptr_t   = L.pointer_type  i8_t
  and tensor_t = L.named_struct_type context "tensor_t" in
  let i8ptr_ptr_t = L.pointer_type i8ptr_t in
  let map_func_t = L.function_type i8ptr_t [|i8ptr_t; i8ptr_t|] 
  and reduce_func_t = L.function_type i8ptr_t [|i8ptr_ptr_t|] in
  let map_func_ptr_t = L.pointer_type map_func_t in
  let map_func_ptr_ptr_t = L.pointer_type map_func_ptr_t
  and reduce_func_ptr_t = L.pointer_type reduce_func_t in
        L.struct_set_body tensor_t [| i8_t; i8_t; i8ptr_t; i8ptr_t; i8_t |] false;

  (* init *)
  let function_type = L.function_type i8_t [||] in
  let main_function = L.define_function "main" function_type the_module in

  let gen_array ltype arr =
      let n = Array.length arr in
      let newarray = if ltype = int_t then Array.make n (L.const_int int_t 0)
                     else Array.make n (L.const_int float_t 0) in
      for i = 0 to (Array.length arr)-1 do
        newarray.(i) <-  
          (match arr.(i) with
            IntLit(x) -> L.const_int ltype x
          | FloatLit(x) -> L.const_float ltype x)
      done; newarray in
    
  let gen_char ltype arr = 
    let n = Array.length arr in
    let newarray = Array.make (n+1) (L.const_int ltype 0) in
    for i = 0 to (Array.length arr)-1 do
        newarray.(i) <-  L.const_int ltype (int_of_char arr.(i))
    done; newarray in

  let gen_dim ltype arr = 
    let n = Array.length arr in
    let newarray = Array.make n (L.const_int ltype 0) in
    for i = 0 to (Array.length arr)-1 do
        newarray.(i) <-  L.const_int ltype arr.(i)
    done; newarray in

  let gen_value ltype value = L.const_int ltype value in

  (* set local pointer *)
  let set_localptr local_builder symbol_table args_name args_val =
    ignore(StringHash.remove symbol_table args_name);
    let alloca = L.build_alloca i8ptr_t args_name local_builder in
        ignore(L.build_store args_val alloca local_builder);
        StringHash.add symbol_table args_name alloca in


  (* create a new tensor *) 
  let build_tensor the_namespace ltype itype dtype ndims dims data =
    let tensor = L.build_malloc tensor_t "raw_tensor" the_namespace.builder in
    (* store dtype *)
    let dtypeptr = L.build_struct_gep tensor 0 "dtype" the_namespace.builder in
    ignore(L.build_store dtype dtypeptr the_namespace.builder);

    (* store ndims *)
    let ndimsptr = L.build_struct_gep tensor 1 "ndims" the_namespace.builder in
    ignore(L.build_store ndims ndimsptr the_namespace.builder);

    (* store dims *)
    let size = Array.length dims in
    let dimsptr = L.build_malloc (L.array_type i64_t size) "dims" the_namespace.builder in
    let dimsptr_as_i8ptr = L.build_bitcast dimsptr i8ptr_t "dims_as_i8ptr" the_namespace.builder in
    let store_dims dim idx = 
      let gep_addr = L.build_gep dimsptr [|L.const_int i64_t 0; L.const_int i64_t idx|] "elmptr" the_namespace.builder in
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
  let subtract_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let subtract_func : L.llvalue = 
    L.declare_function "subtract" subtract_t the_module in
  let negative_t : L.lltype =
    L.function_type i8ptr_t [| i8ptr_t |] in
  let negative_func : L.llvalue =
    L.declare_function "negative" negative_t the_module in
  let mult_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let mult_func : L.llvalue = 
    L.declare_function "mult" mult_t the_module in
  let dotmul_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let dotmul_func : L.llvalue = 
    L.declare_function "dotmul" dotmul_t the_module in
  let divide_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let divide_func : L.llvalue = 
    L.declare_function "divide" divide_t the_module in
  let floordivide_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let floordivide_func : L.llvalue = 
    L.declare_function "floordivide" floordivide_t the_module in
  let matpow_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let matpow_func : L.llvalue = 
    L.declare_function "matpow" matpow_t the_module in
  let dotpow_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let dotpow_func : L.llvalue = 
    L.declare_function "dotpow" dotpow_t the_module in
  let mod_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let mod_func : L.llvalue = 
    L.declare_function "mod" mod_t the_module in
  let transpose_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let transpose_func : L.llvalue = 
    L.declare_function "transpose" transpose_t the_module in
  let equal_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let equal_func : L.llvalue = 
    L.declare_function "equal" equal_t the_module in
  let notequal_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let notequal_func : L.llvalue = 
    L.declare_function "notequal" notequal_t the_module in
  let greater_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let greater_func : L.llvalue = 
    L.declare_function "greater" greater_t the_module in
  let greaterequal_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let greaterequal_func : L.llvalue = 
    L.declare_function "greaterequal" greaterequal_t the_module in
  let less_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let less_func : L.llvalue = 
    L.declare_function "less" less_t the_module in
  let lessequal_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let lessequal_func : L.llvalue = 
    L.declare_function "lessequal" lessequal_t the_module in
  let range_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t; i8ptr_t |] in
  let range_func : L.llvalue = 
    L.declare_function "range" range_t the_module in
  let print_t : L.lltype = 
    L.function_type void_t [| i8ptr_t |] in
  let print_func : L.llvalue = 
    L.declare_function "print" print_t the_module in
  let logicaland_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let logicaland_func : L.llvalue = 
    L.declare_function "logicaland" logicaland_t the_module in
  let logicalor_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t |] in
  let logicalor_func : L.llvalue = 
    L.declare_function "logicalor" logicalor_t the_module in
  let logicalnot_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let logicalnot_func : L.llvalue = 
    L.declare_function "logicalnot" logicalnot_t the_module in
  let len_t : L.lltype = 
    L.function_type int_t [| i8ptr_t |] in
  let len : L.llvalue =
    L.declare_function "len" len_t the_module in

  let zeros_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let zeros_func : L.llvalue = 
    L.declare_function "zeros" zeros_t the_module in
  let cat_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t; i8ptr_t; i8ptr_t |] in
  let cat_func : L.llvalue = 
    L.declare_function "cat" cat_t the_module in
  let shape_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let shape_func : L.llvalue = 
    L.declare_function "shape" shape_t the_module in
  let ones_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let ones_func : L.llvalue = 
    L.declare_function "ones" ones_t the_module in
  let sum_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let sum_func : L.llvalue = 
    L.declare_function "sum" sum_t the_module in
  let any_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let any_func : L.llvalue = 
    L.declare_function "any" any_t the_module in
  let all_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let all_func : L.llvalue = 
    L.declare_function "all" all_t the_module in
  let tensor_abs_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let tensor_abs_func : L.llvalue = 
    L.declare_function "tensor_abs" tensor_abs_t the_module in
  let tensor_log_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let tensor_log_func : L.llvalue = 
    L.declare_function "tensor_log" tensor_log_t the_module in
  let tensor_floor_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let tensor_floor_func : L.llvalue = 
    L.declare_function "tensor_floor" tensor_floor_t the_module in
  let tensor_ceil_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let tensor_ceil_func : L.llvalue = 
    L.declare_function "tensor_ceil" tensor_ceil_t the_module in
  let tensor_round_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let tensor_round_func : L.llvalue = 
    L.declare_function "tensor_round" tensor_round_t the_module in
  let int_of_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let int_of_func : L.llvalue = 
    L.declare_function "int_of" int_of_t the_module in
  let float_of_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let float_of_func : L.llvalue = 
    L.declare_function "float_of" float_of_t the_module in
  let inverse_t : L.lltype = 
    L.function_type i8ptr_t [| i8ptr_t |] in
  let inverse_func : L.llvalue = 
      L.declare_function "inverse" inverse_t the_module in

  let bool_of_zero_t : L.lltype =
    L.function_type bool_t [| i8ptr_t |] in
  let bool_of_zero : L.llvalue = 
    L.declare_function "bool_of_zero" bool_of_zero_t the_module in

  let pe_calc_t : L.lltype = 
  L.function_type i8ptr_t [|map_func_ptr_ptr_t; int_t;  reduce_func_ptr_t; i8ptr_t; i8ptr_t|] in
  let pe_calc : L.llvalue =
  L.declare_function "pe_calc" pe_calc_t the_module in
 
  let index_get_t : L.lltype =
  L.function_type i8ptr_t [|i8ptr_t; i8ptr_t|] in
  let index_get : L.llvalue =
  L.declare_function "index_get" index_get_t the_module in
  let index_get_int_t : L.lltype =
  L.function_type i8ptr_t [|i8ptr_t; int_t|] in
  let index_get_int : L.llvalue =
  L.declare_function "index_get_int" index_get_int_t the_module in
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

  let lookup id the_namespace = if StringHash.mem the_namespace.symbol_table id then StringHash.find the_namespace.symbol_table id
                                else if the_namespace.global then let init = L.define_global id (L.const_pointer_null i8ptr_t) the_module in
                                                                  ignore(StringHash.add the_namespace.symbol_table id init);
                                                                  init
                                else let init = L.build_alloca i8ptr_t id the_namespace.builder in
                                     ignore(L.build_store (L.const_pointer_null i8ptr_t) init the_namespace.builder);
                                     ignore(StringHash.add the_namespace.symbol_table id init);
                                     init in

  let pe_add the_namespace expr1 expr2 = 
    let pe = the_namespace.env in
    let builder = the_namespace.builder in
    let pef = StringHash.find global_pe_table pe in
      match pef.add with
      | DEF -> L.build_call add_func [| expr1 ; expr2 |]
      | PO(po) -> 
        let newpr = L.build_load po.refunc "newpr" builder in
        let newmp = L.build_struct_gep po.mapfuncs 0 "newmp" builder in
        L.build_call pe_calc [| newmp; (L.const_int int_t po.funcnum); newpr ;expr1 ; expr2 |]
  in
  let pe_sub the_namespace expr1 expr2 = 
    let pe = the_namespace.env in
    let builder = the_namespace.builder in
    let pef = StringHash.find global_pe_table pe in
      match pef.minus with
      | DEF -> L.build_call subtract_func [| expr1 ; expr2 |]
      | PO(po) -> 
        let newpr = L.build_load po.refunc "newpr" builder in
        let newmp = L.build_struct_gep po.mapfuncs 0 "newmp" builder in
        L.build_call pe_calc [| newmp; (L.const_int int_t po.funcnum); newpr ;expr1 ; expr2 |]
  in
  let pe_mul the_namespace expr1 expr2 = 
    let pe = the_namespace.env in
    let builder = the_namespace.builder in
    let pef = StringHash.find global_pe_table pe in
      match pef.multi with
      | DEF -> L.build_call mult_func [| expr1 ; expr2 |]
      | PO(po) -> 
        let newpr = L.build_load po.refunc "newpr" builder in
        let newmp = L.build_struct_gep po.mapfuncs 0 "newmp" builder in
        L.build_call pe_calc [| newmp; (L.const_int int_t po.funcnum); newpr ;expr1 ; expr2 |]
  in
  (* expression translation *)
  let rec genExpr the_namespace se = match se with
    | (_, SBinop(se1, op, se2)) -> 
        let se1_ = genExpr the_namespace se1
        and se2_ = genExpr the_namespace se2 in
        (match op with
          Add -> (match the_namespace.env with
          | "default" -> L.build_call add_func [| se1_ ; se2_ |]
          | _ -> pe_add the_namespace se1_ se2_)
        | Sub -> (match the_namespace.env with
          | "default" -> L.build_call subtract_func [| se1_ ; se2_ |]
          | _ -> pe_sub the_namespace se1_ se2_)
        | Mul -> (match the_namespace.env with
          | "default" -> L.build_call mult_func [| se1_ ; se2_ |]
          | _ -> pe_mul the_namespace se1_ se2_)
        | DotMul -> L.build_call dotmul_func [| se1_ ; se2_ |]
        | Div -> L.build_call divide_func [| se1_ ; se2_ |]
        | FlrDiv -> L.build_call floordivide_func [| se1_ ; se2_ |]
        | Pow -> L.build_call matpow_func [| se1_ ; se2_ |]
        | DotPow -> L.build_call dotpow_func [| se1_ ; se2_ |]
        | Mod -> L.build_call mod_func [| se1_ ; se2_ |]
        | Eq -> L.build_call equal_func [| se1_ ; se2_ |]
        | Neq -> L.build_call notequal_func [| se1_ ; se2_ |]
        | Geq -> L.build_call greaterequal_func [| se1_ ; se2_ |]
        | Gt -> L.build_call greater_func [| se1_ ; se2_ |]
        | Leq -> L.build_call lessequal_func [| se1_ ; se2_ |]
        | Lt -> L.build_call less_func [| se1_ ; se2_ |]
        | And -> L.build_call logicaland_func [| se1_ ; se2_ |]
        | Or -> L.build_call logicalor_func [| se1_ ; se2_ |]   
        ) "tmpOp" the_namespace.builder
        (* cast_voidpt_to_tensor the_namespace "tmpOp" tmpOp *)
    | (_, SUnop(uop, se1)) -> 
      let se1_ = genExpr the_namespace se1 in
      (match uop with
        Transpose -> L.build_call transpose_func
      | Not -> L.build_call logicalnot_func
      | Neg -> L.build_call negative_func
      ) [| se1_ |] "tmpOp" the_namespace.builder
    | (STensorTup(t, n, d), STensor(y)) ->
        (match t with 
          INT_Tensor -> build_tensor the_namespace int_t int_t (gen_value i8_t 0) (gen_value i8_t n) (gen_dim i64_t d) (gen_array int_t y)
        | FLOAT_Tensor -> build_tensor the_namespace float_t i64_t (gen_value i8_t 1) (gen_value i8_t n) (gen_dim i64_t d) (gen_array float_t y)
        )
    | (_, SStringLit(s)) -> build_tensor the_namespace i8_t i8_t (gen_value i8_t 2) (gen_value i8_t 1) (gen_dim i64_t [| String.length s |]) (gen_char i8_t (Array.of_seq(String.to_seq s)))
    | (_, SEmptyTensor) -> build_tensor the_namespace i8_t i8_t (gen_value i8_t 21) (gen_value i8_t 0) (gen_dim i64_t [||]) (gen_dim i8_t [||])
    | (_, SVtensor(x)) -> 
      let x_ = Array.of_list(List.map (genExpr the_namespace) x) in
            let dims = gen_dim i64_t [|Array.length(x_)|] in
            let data = x_ in
              build_tensor the_namespace i8ptr_t i64_t (gen_value i8_t 3) (gen_value i8_t 1) dims data 
      (*let rec gen_vartensor = function
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
      in let y = gen_vartensor(x) in y.(0)*)
    | (_, SASexpr(x)) -> (match x with 
        Identifier(id) -> L.build_load (lookup id the_namespace) id the_namespace.builder
      | IdentifierInd(s, x) -> 
        let x_ = Array.of_list(List.map (genExpr the_namespace) x) in
            let dims = gen_dim i64_t [|Array.length(x_)|] in
            let data = x_ in
            let xptr = build_tensor the_namespace i8ptr_t i64_t (gen_value i8_t 3) (gen_value i8_t 1) dims data in
            let sptr = L.build_load (lookup s the_namespace) s the_namespace.builder in
            L.build_call index_get [|sptr; xptr|] "access_tensor" the_namespace.builder
        (*(match x with
          (nlist, indlist) -> let rec gen_indlist = function
            [] -> [||]
          | (n, d, y)::indlist_ -> let i0 = genExpr the_namespace (STensorTup(INT_Tensor, n, d), STensor(y)) in 
            let y1 = [|i0|] and y2 = gen_indlist(indlist_) in Array.append y1 y2 in 
            let dims = gen_dim i8_t [|nlist|] in
            let data = gen_indlist(indlist) in
            let xptr = build_tensor the_namespace i8ptr_t i64_t (gen_value i8_t 3) (gen_value i8_t 1) dims data in
            let sptr = L.build_load (lookup s the_namespace) s the_namespace.builder in
            L.build_call index_get [|sptr; xptr|] "access_tensor" the_namespace.builder
          )*)
      )
    | (_, SRange(se1, se2, se3)) -> 
              let se1_ = genExpr the_namespace se1 in 
              let se2_ = genExpr the_namespace se2 in 
              let se3_ = genExpr the_namespace se3 in 
              L.build_call range_func [| se1_; se2_; se3_ |] "tmpOp" the_namespace.builder
    | (_, SPrint(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call print_func [| se1_ |] "" the_namespace.builder
    | (_, SZeros(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call zeros_func [| se1_ |] "zeros" the_namespace.builder
    | (_, SShape(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call shape_func [| se1_ |] "shape" the_namespace.builder
    | (_, SOnes(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call ones_func [| se1_ |] "ones" the_namespace.builder
    | (_, SSum(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call sum_func [| se1_ |] "sum" the_namespace.builder
    | (_, SAny(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call any_func [| se1_ |] "any" the_namespace.builder
    | (_, SAll(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call all_func [| se1_ |] "all" the_namespace.builder
    | (_, SAbs(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call tensor_abs_func [| se1_ |] "abs" the_namespace.builder
    | (_, SLog(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call tensor_log_func [| se1_ |] "log" the_namespace.builder
    | (_, SInt_Of(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call int_of_func [| se1_ |] "int_of" the_namespace.builder
    | (_, SFloat_Of(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call float_of_func [| se1_ |] "float_of" the_namespace.builder
    | (_, SFloor(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call tensor_floor_func [| se1_ |] "abs" the_namespace.builder
    | (_, SCeil(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call tensor_ceil_func [| se1_ |] "ceil" the_namespace.builder
    | (_, SRound(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call tensor_round_func [| se1_ |] "round" the_namespace.builder
    | (_, SInverse(se1)) -> let se1_ = genExpr the_namespace se1 in
                          L.build_call inverse_func [| se1_ |] "inverse" the_namespace.builder
    | (_, SCat(se1, se2, se3)) -> let se1_ = genExpr the_namespace se1 
                                  and se2_ = genExpr the_namespace se2 
                                  and se3_ = genExpr the_namespace se3 in
                          L.build_call cat_func [| se1_ ; se2_ ; se3_ |] "cat" the_namespace.builder
    | (_, SFuncCall(str1, se1)) -> let (the_function, _) = StringHash.find the_namespace.function_table str1 in
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
              Identifier(id) ->
                          let rhs = genExpr the_namespace se1 in
                          let lhs = lookup id the_namespace in
                          let lhsptr = L.build_load lhs "lhsptr" the_namespace.builder in
                          ignore(L.build_call increase_rc [| rhs |] "" the_namespace.builder);
                          ignore(L.build_call decrease_rc [| lhsptr |] "" the_namespace.builder);
                          ignore(L.build_store rhs lhs the_namespace.builder);
                          the_namespace
            | IdentifierInd(s, x) ->
                          let rhs = genExpr the_namespace se1 in
                          let x_ = Array.of_list(List.map (genExpr the_namespace) x) in
                            let dims = gen_dim i64_t [|Array.length(x_)|] in
                            let data = x_ in
                            let xptr = build_tensor the_namespace i8ptr_t i64_t (gen_value i8_t 3) (gen_value i8_t 1) dims data in
                            let sptr = L.build_load (lookup s the_namespace) s the_namespace.builder in
                            ignore(L.build_call index_put [|sptr; xptr; rhs|] "" the_namespace.builder);
                            the_namespace
                  (*(match x with
                          (nlist, indlist) -> let rec gen_indlist = function
                            [] -> [||]
                          | (n, d, y)::indlist_ -> let i0 = genExpr the_namespace (STensorTup(INT_Tensor, n, d), STensor(y)) in 
                            let y1 = [|i0|] and y2 = gen_indlist(indlist_) in Array.append y1 y2 in 
                            let dims = gen_dim i8_t [|nlist|] in
                            let data = gen_indlist(indlist) in
                            let xptr = build_tensor the_namespace i8ptr_t i64_t (gen_value i8_t 3) (gen_value i8_t 1) dims data in
                            let sptr = L.build_load (lookup s the_namespace) s the_namespace.builder in
                            L.build_call index_put [|sptr; xptr; rhs|] "" the_namespace.builder
                  ); the_namespace*)
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
                             let local_namespace = {symbol_table = local_symbol_table; function_table = local_function_table; func = the_function; builder = the_builder; global = false; env = the_namespace.env} in
                             ignore(List.fold_left stmt local_namespace ss1);
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
                                                       global = the_namespace.global;
                                                       env = the_namespace.env} in
                                let local_namespace = List.fold_left stmt local_namespace ss1 in
                                ignore(add_terminal local_namespace.builder build_br_merge);
                                let else_bb = L.append_block context "else" the_namespace.func in
                                let local_builder = L.builder_at_end context else_bb in
                                let local_namespace = {symbol_table = the_namespace.symbol_table; 
                                                       function_table = the_namespace.function_table;
                                                       func = the_namespace.func; 
                                                       builder = local_builder;
                                                       global = the_namespace.global;
                                                       env = the_namespace.env} in
                                let local_namespace = List.fold_left stmt local_namespace ss2 in
                                ignore(add_terminal local_namespace.builder build_br_merge);
                                ignore(L.build_cond_br bool_val then_bb else_bb the_namespace.builder);
                                let builder = L.builder_at_end context merge_bb in
                                {symbol_table = the_namespace.symbol_table; function_table = the_namespace.function_table; func = the_namespace.func; builder = builder; global = the_namespace.global; env = the_namespace.env}
    | SWhileStmt(se1, ss1) -> let pred_bb = L.append_block context "while" the_namespace.func in
                              ignore(L.build_br pred_bb the_namespace.builder);
                              let body_bb = L.append_block context "while_body" the_namespace.func in
                              let local_builder = L.builder_at_end context body_bb in
                              let local_namespace = {symbol_table = the_namespace.symbol_table; 
                                                     function_table = the_namespace.function_table;
                                                     func = the_namespace.func; 
                                                     builder = local_builder;
                                                     global = the_namespace.global;
                                                     env = the_namespace.env} in
                              let local_namespace = List.fold_left stmt local_namespace ss1 in
                              ignore(add_terminal local_namespace.builder (L.build_br pred_bb));
                              let pred_builder = L.builder_at_end context pred_bb in
                              let local_namespace = {symbol_table = the_namespace.symbol_table;
                                                     function_table = the_namespace.function_table;
                                                     func = the_namespace.func;
                                                     builder = pred_builder;
                                                     global = the_namespace.global;
                                                     env = the_namespace.env} in
                              let se1_ = genExpr local_namespace se1 in
                              let bool_val = L.build_call bool_of_zero [| se1_ |] "bool" local_namespace.builder in
                              let merge_bb = L.append_block context "merge" the_namespace.func in
                              ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
                              let builder = L.builder_at_end context merge_bb in
                              {symbol_table = the_namespace.symbol_table; function_table = the_namespace.function_table; func = the_namespace.func; builder = builder; global = the_namespace.global;env = the_namespace.env}
    | SForStmt(str1, se1, ss1) -> let idxptr = L.build_alloca int_t "idxptr" the_namespace.builder in
                                  ignore(L.build_store (L.const_int int_t (0)) idxptr the_namespace.builder);
                                  let tensor = genExpr the_namespace se1 in
                                  let size = L.build_call len [| tensor |] "length" the_namespace.builder in
                                  let pred_bb = L.append_block context "for" the_namespace.func in
                                  ignore(L.build_br pred_bb the_namespace.builder);
                                  let pred_builder = L.builder_at_end context pred_bb in
                                  let indicator = L.build_load idxptr "idx" pred_builder in
                                  let new_indicator = L.build_add indicator (L.const_int int_t 1) "new_idx"  pred_builder in
                                  ignore(L.build_store new_indicator idxptr pred_builder);
                                  let cond = (L.build_icmp L.Icmp.Sgt) new_indicator size "condition" pred_builder in

                                  let body_bb = L.append_block context "for_body" the_namespace.func in
                                  let body_builder = L.builder_at_end context body_bb in
                                  let local_namespace = {symbol_table = the_namespace.symbol_table; 
                                                        function_table = the_namespace.function_table;
                                                        func = the_namespace.func; 
                                                        builder = body_builder;
                                                        global = the_namespace.global;
                                                        env = the_namespace.env} in
                                  (* let indicator_as_tensor = build_tensor local_namespace int_t int_t (gen_value i8_t 0) (gen_value i8_t 1) [| gen_value i8_t 1 |] [| indicator |] in *)
                                  let str1ptr = lookup str1 local_namespace in
                                  let indicator_as_tensor = L.build_call index_get_int [|tensor; indicator|] "indicator_as_tensor" body_builder in
                                  ignore(L.build_store indicator_as_tensor str1ptr body_builder);
                                  let local_namespace = List.fold_left stmt local_namespace ss1 in
                                  ignore(add_terminal local_namespace.builder (L.build_br pred_bb));

                                  let merge_bb = L.append_block context "merge" the_namespace.func in
                                  ignore(L.build_cond_br cond merge_bb body_bb pred_builder);
                                  let builder = L.builder_at_end context merge_bb in
                                  {symbol_table = the_namespace.symbol_table; function_table = the_namespace.function_table; func = the_namespace.func; builder = builder; global = the_namespace.global; env = the_namespace.env}
    | SReturn(se1) -> ignore(L.build_ret (genExpr the_namespace se1) the_namespace.builder); the_namespace
    | SPEInvoke(str1) -> {symbol_table = the_namespace.symbol_table; function_table = the_namespace.function_table; func = the_namespace.func; builder = the_namespace.builder; global = the_namespace.global; env = str1}
    | SPEEnd(_) -> {symbol_table = the_namespace.symbol_table; function_table = the_namespace.function_table; func = the_namespace.func; builder = the_namespace.builder; global = the_namespace.global; env = "default"}
  
  and pedecl main_namespace (pename, pebody) =
      let map_value_helper bigname po_map_array idx (name, _) = 
        let fname = bigname ^ name in
        let tmp = L.build_struct_gep po_map_array idx "tmp" main_namespace.builder in
        let ftmp = match (L.lookup_function fname the_module) with 
                      Some z-> z 
                    | _ -> raise (Failure("Bug"))in
        ignore(L.build_store ftmp tmp main_namespace.builder);
        idx+1
      in

      let pofp_creator pename pofunc =
        let poname = pename ^ pofunc.soperator in
        let funcn = List.length(pofunc.smapfuncs) in 
        let po_map_id = poname ^ "maps" in
        let po_map_array = L.define_global po_map_id (L.const_null (L.array_type map_func_ptr_t funcn)) the_module in
        (* ignore(StringHash.add main_namespace.symbol_table po_map_id po_map_array); *)
        ignore(List.fold_left (map_value_helper poname po_map_array) 0 pofunc.smapfuncs);
        let po_reduce_id = poname ^ "reduce" in 
        let po_reduce = L.define_global po_reduce_id (L.const_pointer_null reduce_func_ptr_t) the_module in
        (* ignore(StringHash.add main_namespace.symbol_table po_reduce_id po_reduce); *)
        let ftmp = match (L.lookup_function (poname ^ "reduce") the_module) with 
                    Some z-> z 
                  | _ -> raise (Failure("Bug")) in
        ignore(L.build_store ftmp po_reduce main_namespace.builder);
        PO({
          funcnum = funcn;
          mapfuncs = po_map_array;
          refunc = po_reduce;
        })
      in
      let build_pofunc bigname paras (name, stmts) =
        let fname = bigname ^ name in
        let (the_function, the_builder) = build_fn fname 2 in
        let local_symbol_table = StringHash.create 10 in
        let local_function_table = StringHash.create 10 in
        let argv = Array.to_list (L.params the_function) in
        List.iter2 (set_localptr the_builder local_symbol_table) paras argv;
        let build_return b = L.build_ret (L.const_pointer_null i8ptr_t) b in
        let local_namespace = {symbol_table = local_symbol_table; function_table = local_function_table; func = the_function; builder = the_builder; global = false; env = "default"} in
        ignore(List.fold_left stmt local_namespace stmts);
        ignore(add_terminal the_builder build_return); 
      in 
      let reduce_value_helper local_symbol_table the_builder reduce_array idx name = 
        let alloca = L.build_alloca i8ptr_t name the_builder in
        let newa = L.build_load reduce_array "newa" the_builder in
        let tmp = L.build_gep newa [|(L.const_int int_t idx)|] "tmp" the_builder in
        let newv = L.build_load tmp "newv" the_builder in
        ignore(L.build_store newv alloca the_builder);
        ignore(StringHash.add local_symbol_table name alloca);
        idx+1
      in

      let build_reducefunc name vars stmts =
        let the_function = L.define_function name reduce_func_t the_module in
        let the_builder = L.builder_at_end context (L.entry_block the_function) in
        let local_symbol_table = StringHash.create 10 in
        let local_function_table = StringHash.create 10 in

        let argval = List.hd(Array.to_list (L.params the_function)) in
        ignore(L.set_value_name "result" argval);
        let alloca = L.build_alloca i8ptr_ptr_t "result" the_builder in
        ignore(L.build_store argval alloca the_builder);

        ignore(List.fold_left (reduce_value_helper local_symbol_table the_builder alloca) 0 vars);

        let build_return b = L.build_ret (L.const_pointer_null i8ptr_t) b in
        let local_namespace = {symbol_table = local_symbol_table; function_table = local_function_table; func = the_function; builder = the_builder; global = false; env = "default"} in
        ignore(List.fold_left stmt local_namespace stmts);
        ignore(add_terminal the_builder build_return);
      in
      let pogenerate pename pofunc =
        let poname = pename ^ pofunc.soperator in
        ignore(List.iter (build_pofunc poname pofunc.sparams) pofunc.smapfuncs);
        ignore(build_reducefunc (poname ^ "reduce") (List.map (fun (name, _) -> name) pofunc.smapfuncs) pofunc.sreducefunc);
        pofp_creator pename pofunc
      in
      let podecl pename pof = 
      match pof with
        SDEF -> DEF
      | SPO(p) -> pogenerate pename p
      in
      ignore(StringHash.add global_pe_table pename
      {
        add = podecl pename pebody.sadd;
        minus = podecl pename pebody.sminus;
        multi = podecl pename pebody.smulti;
      });
      main_namespace

  in

  let main_builder = L.builder_at_end context (L.entry_block main_function) in
  let main_namespace = {symbol_table = global_symbol_table; function_table = global_function_table; func = main_function; builder = main_builder; global = true; env = "default"} in
  let main_namespace = List.fold_left pedecl main_namespace spes in
  let main_namespace = List.fold_left stmt main_namespace sstmts in
  ignore(L.build_ret (L.const_int i8_t 0) main_namespace.builder);
  the_module;