(* Writes the blocks of OCaml of a Markdown page to stdout as one program, each block a module of its own, so that the
   page's examples are built and run as a test (getting_started.ml, by the rule in dune). *)

let () =
  let ic = open_in_bin Sys.argv.(1) in
  let n = in_channel_length ic in
  let page = really_input_string ic n in
  close_in ic;
  let lines = String.split_on_char '\n' page in
  let rec scan inside count = function
    | [] -> if count < 5 then failwith (Printf.sprintf "the page has %d examples" count)
    | "```ocaml" :: rest ->
        Printf.printf "module Example_%d = struct\n" (count + 1);
        scan true (count + 1) rest
    | "```" :: rest when inside ->
        print_string "end\n\n";
        scan false count rest
    | line :: rest ->
        if inside then (print_string line; print_char '\n');
        scan inside count rest
  in
  scan false 0 lines
