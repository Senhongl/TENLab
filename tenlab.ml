let _ =
  let lexbuf = Lexing.from_channel stdin in
  let stmts = Parser.main Scanner.tokenize lexbuf in
  print_endline (Ast.string_of_program stmts)
