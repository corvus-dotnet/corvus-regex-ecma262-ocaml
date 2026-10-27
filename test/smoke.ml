module R = Corvus_regex_ecma262

let show re s =
  match R.find re s with
  | None -> print_endline (Printf.sprintf "/%s/%s %S: none" (R.pattern re) (R.flags_string re) s)
  | Some m ->
      Printf.printf "/%s/%s %S: %d-%d %S" (R.pattern re) (R.flags_string re) s (R.Match.start m) (R.Match.end_ m)
        (R.Match.value m);
      for g = 1 to R.Match.group_count m do
        Printf.printf " [%d]=%s" g (match R.Match.group m g with Some v -> Printf.sprintf "%S" v | None -> "undefined")
      done;
      print_newline ()

let () =
  let c p f = R.compile_exn ~flags:f p in
  show (c "(?<year>\\d{4})-(?<month>\\d{2})" "u") "on 2026-10-27";
  show (c "a+b" "") "xxaaab";
  show (c "(a|ab)(c|bcd)(d*)" "") "abcd";
  show (c "\\p{Lu}+" "u") "abcÉTÉx";
  show (c "^(?:(a+)\\1*)+$" "") "aaaa";
  show (c "(?<=\\$)\\d+(\\.\\d*)?" "") "cost $10.50";
  show (c "(?i:ſ)" "u") "S";
  show (c "[[a-z]--[aeiou]]+" "v") "aeixyzo";
  show (c "\\b\\w+\\b" "g") "  hello world";
  Printf.printf "count %d\n" (R.count (c "\\d+" "g") "1 22 333 4444");
  print_endline (R.replace (c "(\\w+) (\\w+)" "") "Ada Lovelace" "$2, $1");
  print_endline (R.replace_all (c "o" "") "foo boo" "0");
  print_endline
    (String.concat "|" (List.map (function Some s -> s | None -> "undef") (R.split (c "\\s*(,)\\s*" "") "a , b,c")));
  (match R.compile ~flags:"u" "(" with Error e -> print_endline (R.error_message e) | Ok _ -> ());
  (match R.compile ~flags:"uv" "a" with Error e -> print_endline (R.error_message e) | Ok _ -> ());
  let re = R.compile_exn ~match_timeout:0.05 "^(?:(a+)\\1*)+$" in
  (try ignore (R.is_match re (String.make 60 'a' ^ "b")); print_endline "no timeout?" with R.Match_timeout -> print_endline "timeout ok");
  (* UTF-16: "a😀b" *)
  let u16 = "a\x00\x3d\xd8\x00\xdeb\x00" in
  (match R.find_utf16 (R.compile_exn ~flags:"u" ".") (String.sub u16 2 6) with Some m -> Printf.printf "utf16 . -> %d-%d\n" (R.Match.start m) (R.Match.end_ m) | None -> ());
  (match R.find_utf16 (R.compile_exn ".") (String.sub u16 2 6) with Some m -> Printf.printf "utf16 . (no u) -> %d-%d\n" (R.Match.start m) (R.Match.end_ m) | None -> ())
