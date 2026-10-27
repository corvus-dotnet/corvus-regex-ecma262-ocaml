module R = Corvus_regex_ecma262

let () =
  let re = R.compile_exn ~flags:"u" {|(?<year>\d{4})-(?<month>\d{2})|} in
  assert (R.is_match re "on 2026-10-25");
  assert (R.replace re "2026-10-25" "$<month>/$<year>" = "10/2026-25");
  (match R.find re "on 2026-10-25" with
   | Some m -> assert (R.Match.named m "month" = Some "10" && R.Match.start m = 3)
   | None -> assert false);
  (* A pattern from outside: no exception, and a timeout of its own. *)
  match R.compile ~flags:"u" ~match_timeout:0.1 "[a-z" with
  | Error e -> print_endline (R.error_message e)
  | Ok _ -> ()
