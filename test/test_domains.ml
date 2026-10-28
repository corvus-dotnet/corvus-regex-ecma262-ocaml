(* The first use of a version of Unicode from many domains at once. Each version's tables, folds and property sets
   are made when first asked for; this asks for those of Unicode 13 and 18 from eight domains together, before
   anything else has, and checks that every domain gets the same answers as one alone. OCaml 5 only. *)

module R = Corvus_regex_ecma262

let vithkuqi = "\xF0\x90\x95\xB0\xF0\x90\x96\x97"

let answers () =
  let r13 = R.compile_exn ~flags:"iu" ~unicode_version:R.V13 "^(?:\\p{Ll}|\\p{Script=Yezidi}|(.)\\1)$" in
  let r18 = R.compile_exn ~flags:"iu" ~unicode_version:R.V18 "^(?:\\p{Ll}|\\p{Script=Jurchen}|(.)\\1)$" in
  let r18s = R.compile_exn ~flags:"v" ~unicode_version:R.V18 "^\\p{RGI_Emoji}$" in
  [ R.is_match r13 vithkuqi; R.is_match r13 "\xF0\x90\xBA\x80"; R.is_match r18 vithkuqi; R.is_match r18 "\xF0\x98\xB8\x80";
    R.is_match r18s "\xF0\x9F\x98\x80" ]

let () =
  let go = Atomic.make false in
  let domains =
    List.init 8 (fun _ ->
        Domain.spawn (fun () ->
            while not (Atomic.get go) do
              Domain.cpu_relax ()
            done;
            answers ()))
  in
  Atomic.set go true;
  let got = List.map Domain.join domains in
  (* Unicode 13 has no Vithkuqi, so its capital and small letters are neither letters nor one letter folded. *)
  let want = [ false; true; true; true; true ] in
  let failed = List.length (List.filter (fun g -> g <> want) got) in
  List.iter
    (fun g -> if g <> want then print_endline ("FAIL: " ^ String.concat " " (List.map string_of_bool g)))
    got;
  Printf.printf "TestDomains: %d domains, %d failed\n" (List.length got) failed;
  if failed > 0 then exit 1
