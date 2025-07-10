(* main.ml *)

let () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0);
    exit 1
  );
  let filename = Sys.argv.(1) in
  let content = In_channel.with_open_bin filename In_channel.input_all in
  let tokens = Lib.Lexer.tokenize content in
  List.iter print_endline tokens
