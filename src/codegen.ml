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
module A = Ast
open Sast 

let translate sstmts =
  let context = L.global_context() in

  let the_module = L.create_module context "TENLab" in

  (* TODO: How to support tensor? Seems like it can be supported directly. *)
  let i32_t   = L.i32_type      context
  and i8_t    = L.i8_type       context
  and float_t = L.double_type   context
  and void_t  = L.void_type     context  in
  let str_t   = L.pointer_type  i8_t     in

  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Float -> float_t
    | A.String -> str_t
    | A.Void  -> void_t
  in

  let function_type = L.function_type i8_t [||] in
  let the_function = L.define_function "main" function_type the_module in

  let printf_t : L.lltype = 
      L.var_arg_function_type void_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  let rec expr builder e = match e with
      SLit lit -> (match lit with
          A.IntLit    i -> L.const_int    i32_t   i
        | A.FloatLit  f -> L.const_float  float_t f
        | A.StringLit s -> L.build_global_stringptr (s ^ "\n") "string_ptr" builder)
    | SPrint se -> L.build_call printf_func [| expr builder se |] "" builder in
  
  let rec stmt builder = function
      SExpr se -> ignore(expr builder se); builder in

  let builder = L.builder_at_end context (L.entry_block the_function) in
  let builder = List.fold_left stmt builder sstmts in
  ignore(L.build_ret (L.const_int i8_t 0) builder);
  the_module;