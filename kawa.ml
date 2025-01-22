open Format






let () =
  let file = Sys.argv.(1) in
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  try
    let prog = Parser.prog Lexer.token lb in
    close_in c;
    Type_checker.typecheck_program prog; 
    Interpreter.exec_prog prog;
    print_string "\nyoupi\n";
    exit 0
  with
  | Parser.Error ->
     eprintf "syntax error@.";
     exit 1
  | Type_checker.UnificationImpossible(t1,t2) ->
     eprintf "unification impossible entre %a et %a \n" Type.pp_typ t1 Type.pp_typ t2
  | e ->
     eprintf "erreur non-attendue %s\n@." (Printexc.to_string e);
     exit 2
