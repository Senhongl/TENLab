module L = Llvm
module La = Llvm_analysis
open Tensorsast
open Tensorast

(*let rec print_tensor = function 
  [] -> print_char(')')
| IntLit(e)::l -> print_int e; print_string ","; print_tensor l
| FloatLit(e)::l -> print_float e; print_string ","; print_tensor l

let print_dimtype = function
  VoidTup -> print_string("i8 *(")
| TensorTup(t, n, d) -> print_string("Tensor *(")

let print_op = function
  Add -> print_string("+")
| Mul -> print_string("*")

let rec print_sast = function
  (t, SBinop(x1, op, x2)) -> print_dimtype t; 
    print_sast x1; print_op op; print_sast x2; print_char(')')
| (t, STensor(x)) -> print_dimtype t; print_tensor(Array.to_list x)*)

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

let translate sast =
  let context = L.global_context() in
  let the_module = L.create_module context "TensorC" in 

    let i8_t = L.i8_type context 
    and int_t = L.i32_type context 
    and float_t = L.float_type context 
    and void_t = L.void_type context in
    let i8ptr_t = L.pointer_type i8_t
    and tensor_t = L.named_struct_type context "tensor_t" in
      L.struct_set_body tensor_t [| i8_t; i8_t; i8ptr_t; i8ptr_t |] false;
  
  let function_type = L.function_type i8_t [||] in
  let the_function = L.define_function "main" function_type the_module in

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
    | (_, SVtensor(x)) -> let rec gen_vartensor = function
        Tensor0(x) -> (match x with
          IntLit(y) -> let ptrx = set_constptr "stensor" (L.const_named_struct tensor_t 
            (let dims = set_constptr "sdim" (gen_dim i8_t [||]) the_module i8ptr_t
            and data = set_constptr "sdata" (gen_array int_t [|x|]) the_module i8ptr_t
            in [|gen_value i8_t 0; gen_value i8_t 0; dims; data|])) the_module i8ptr_t
          in [|ptrx|]
        | FloatLit(y) -> let ptrx = set_constptr "stensor" (L.const_named_struct tensor_t 
            (let dims = set_constptr "sdim" (gen_dim i8_t [||]) the_module i8ptr_t
            and data = set_constptr "sdata" (gen_array float_t [|x|]) the_module i8ptr_t
            in [|gen_value i8_t 1; gen_value i8_t 0; dims; data|])) the_module i8ptr_t
          in [|ptrx|])
      | LRTensor(x) -> let y = gen_vartensor(x) in
            let ptrx = set_constptr "stensor" (L.const_named_struct tensor_t 
            (let dims = set_constptr "sdim" (gen_dim i8_t [|Array.length(y)|]) the_module i8ptr_t
            and data = set_constptr "sdata" (L.const_array i8ptr_t y) the_module i8ptr_t
            in [|gen_value i8_t 3; gen_value i8_t 1; dims; data|])) the_module i8ptr_t
          in [|ptrx|]
      | LRTensors(x1, x2) -> let y1 = gen_vartensor(x1) and y2 = gen_vartensor(x2) in
            let ptrx = set_constptr "stensor" (L.const_named_struct tensor_t 
            (let dims = set_constptr "sdim" (gen_dim i8_t [|Array.length(y1)+Array.length(y2)|]) the_module i8ptr_t
            and data = set_constptr "sdata" (L.const_array i8ptr_t (Array.append y1 y2)) the_module i8ptr_t
            in [|gen_value i8_t 3; gen_value i8_t 1; dims; data|])) the_module i8ptr_t
          in [|ptrx|]
      | NPTensor(x) -> gen_vartensor(x)
      | NPTensors(x1, x2) -> let y1 = gen_vartensor(x1) and y2 = gen_vartensor(x2) in 
        Array.append y1 y2
      in let y = gen_vartensor(x) in y.(0)
    | (_, SASexpr(x)) -> (match x with 
        Ident(s) -> gen_value i8ptr_t 0 (*Do something*)
      | Idind(s, x) -> (match x with
          (nlist, indlist) -> let rec gen_indlist = function
            [] -> [||]
          | (n, d, y)::indlist_ -> let i0 = genExpr builder (STensorTup(INT_Tensor, n, d), STensor(y)) in 
            let y1 = [|i0|] and y2 = gen_indlist(indlist_) in Array.append y1 y2 in 
            set_constptr "stensor" (L.const_named_struct tensor_t 
            (let dims = set_constptr "sdim" (gen_dim i8_t [|nlist|]) the_module i8ptr_t
            and data = set_constptr "sdata" (L.const_array i8ptr_t (gen_indlist(indlist))) the_module i8ptr_t
            in [|gen_value i8_t 3; gen_value i8_t 1; dims; data|])) the_module i8ptr_t)
      )
    | (_, _) -> gen_value i8ptr_t 0 in

  let builder = L.builder_at_end context (L.entry_block the_function) in
  let builder = ignore(L.build_call print_func [|(genExpr builder sast)|] "" builder); builder in
  ignore(L.build_ret (L.const_int i8_t 0) builder);
  the_module


let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Tensorparser.expr Tensorscanner.tokenize lexbuf in
  let sast = Tensorsemant.check_expr ast in 
    let m = translate sast in
	    La.assert_valid_module m;
	    print_string (L.string_of_llmodule m)