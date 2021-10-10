let () =
  let fname = Sys.argv.(1) in
  let prog =
    let lexbuf = Lexing.from_channel (open_in fname) in
    try
      Parser.prog Lexer.token lexbuf
    with
    | Parser.Error ->
      let pos = (Lexing.lexeme_end_p lexbuf) in
      Printf.printf
        "Parsing error in file %s at word \"%s\", line %d, character %d.\n%!"
        pos.Lexing.pos_fname
        (Lexing.lexeme lexbuf)
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol - 1);
      exit 1

  in
  Printf.printf "# Parsing\n\n%!";
  List.iter (fun (r,x,t) -> Printf.printf "let%s %s = %s\n%!" (if r then " rec" else "") x (Lang.to_string t)) prog;
  Printf.printf "\n# Type inference\n\n%!";
  let _ =
    List.fold_left
      (fun env (_,x,t) ->
         let a = Type.infer env t in
         Printf.printf "%s : %s\n%!" x (Type.to_string a);
         (x,a)::env
      ) [] prog
  in
  ()
