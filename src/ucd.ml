(* The Unicode Character Database properties the engine reads, for one fixed version of Unicode (ucd.rs of the Rust
   crate).

   Nothing in the engine takes Unicode properties from the standard library or the platform, which follow whatever
   built the program. With its own tables the library gives the same answers whatever builds it, and the answers
   change only when the tables are generated again. Ucd_tables.version names the version of Unicode they hold.

   The tables are in ucd_tables.ml, which tools/unicode/gen-ucd-tables.ps1 writes from the tables of the Rust crate,
   so every engine holds the same data. Each is a sorted list of inclusive ranges, the code points up to U+FFFF in
   r16 and the rest in r32. A value that follows from other values (a General_Category group such as L, the Assigned
   property, the Unknown script) has no table of its own and is built when it is first asked for. *)

type table = Ucd_tables.table

(* The last Unicode code point. *)
let max_rune = 0x10FFFF

let search ranges c =
  let lo = ref 0 and hi = ref (Array.length ranges / 2) and found = ref false in
  while (not !found) && !lo < !hi do
    let mid = (!lo + !hi) lsr 1 in
    if c > ranges.((2 * mid) + 1) then lo := mid + 1
    else if c < ranges.(2 * mid) then hi := mid
    else found := true
  done;
  !found

(* Whether the table holds the code point. *)
let contains (t : table) r = if r <= 0xFFFF then search t.r16 r else search t.r32 r

(* The ranges of the table as inclusive pairs in ascending order. The pairs neither overlap nor touch: a range that
   crosses U+FFFF is stored in two parts, and is one here. *)
let ranges (t : table) =
  let n16 = Array.length t.r16 and n32 = Array.length t.r32 in
  if n16 > 0 && n32 > 0 && t.r16.(n16 - 1) = 0xFFFF && t.r32.(0) = 0x10000 then begin
    let out = Array.make (n16 + n32 - 2) 0 in
    Array.blit t.r16 0 out 0 n16;
    out.(n16 - 1) <- t.r32.(1);
    Array.blit t.r32 2 out n16 (n32 - 2);
    out
  end
  else Array.append t.r16 t.r32

(* The ranges of a property: of a generated table, or of one that was built (as pairs). *)
type ranges = Table of table | Built of int array Once.t

(* The ranges as inclusive pairs in ascending order. *)
let pairs = function Table t -> ranges t | Built b -> Array.copy (Once.get b)

(* The ranges of the code points that any of the tables holds. *)
let union tables =
  let all = Array.concat (List.map ranges tables) in
  let order = Array.init (Array.length all / 2) Fun.id in
  Array.stable_sort (fun a b -> compare all.(2 * a) all.(2 * b)) order;
  let merged = Buffer_int.create (Array.length all) in
  Array.iter
    (fun i ->
      let lo = all.(2 * i) and hi = all.((2 * i) + 1) in
      let n = Buffer_int.length merged in
      if n > 0 && lo <= Buffer_int.get merged (n - 1) + 1 then begin
        if hi > Buffer_int.get merged (n - 1) then Buffer_int.set merged (n - 1) hi
      end
      else begin
        Buffer_int.add merged lo;
        Buffer_int.add merged hi
      end)
    order;
  Buffer_int.contents merged

(* The ranges of the code points that the table does not hold. *)
let complement t =
  let p = ranges t in
  let out = Buffer_int.create (Array.length p + 2) in
  let next = ref 0 in
  for k = 0 to (Array.length p / 2) - 1 do
    let lo = p.(2 * k) and hi = p.((2 * k) + 1) in
    if lo > !next then begin
      Buffer_int.add out !next;
      Buffer_int.add out (lo - 1)
    end;
    next := hi + 1
  done;
  if !next <= max_rune then begin
    Buffer_int.add out !next;
    Buffer_int.add out max_rune
  end;
  Buffer_int.contents out

open Ucd_tables

(* The General_Category values that are groups of other values, the Assigned property and the Unknown script. Each is
   built on first use and kept. *)
let letter = Once.make (fun () -> union [ gc_lu; gc_ll; gc_lt; gc_lm; gc_lo ])
let cased_letter = Once.make (fun () -> union [ gc_lu; gc_ll; gc_lt ])
let mark = Once.make (fun () -> union [ gc_mn; gc_mc; gc_me ])
let number = Once.make (fun () -> union [ gc_nd; gc_nl; gc_no ])
let punctuation = Once.make (fun () -> union [ gc_pc; gc_pd; gc_ps; gc_pe; gc_pi; gc_pf; gc_po ])
let symbol = Once.make (fun () -> union [ gc_sm; gc_sc; gc_sk; gc_so ])
let separator = Once.make (fun () -> union [ gc_zs; gc_zl; gc_zp ])
let other = Once.make (fun () -> union [ gc_cc; gc_cf; gc_cs; gc_co; gc_cn ])

(* The code points that have a General_Category other than Cn. *)
let assigned = Once.make (fun () -> complement gc_cn)

(* The code points that belong to no script: the unassigned, private use and surrogate code points. *)
let unknown_script = Once.make (fun () -> union [ gc_cn; gc_co; gc_cs ])

(* The table of the Script value Katakana_Or_Hiragana (Hrkt). PropertyValueAliases.txt lists the value, and no code
   point has it, in Script or in Script_Extensions. Scripts.txt has no line for it, so the generated tables do not
   have it. *)
let katakana_or_hiragana = { r16 = [||]; r32 = [||] }

(* The table of a General_Category value by its short name, such as "Lu" or "L". None for any other name. *)
let category name =
  match name with
  | "L" -> Some (Built letter)
  | "LC" -> Some (Built cased_letter)
  | "M" -> Some (Built mark)
  | "N" -> Some (Built number)
  | "P" -> Some (Built punctuation)
  | "S" -> Some (Built symbol)
  | "Z" -> Some (Built separator)
  | "C" -> Some (Built other)
  | _ -> Option.map (fun t -> Table t) (category_table name)

(* The table of a binary property by its canonical name, such as "Alphabetic" or "White_Space". None for any other
   name. ASCII and Any, which ECMA-262 adds to the properties of Unicode, are not here. *)
let binary name =
  if name = "Assigned" then Some (Built assigned) else Option.map (fun t -> Table t) (binary_table name)

(* The table of a Script value by its long name or an alias, such as "Greek" or "Grek". None for any other name. *)
let script name =
  match name with
  | "Unknown" | "Zzzz" -> Some (Built unknown_script)
  | "Katakana_Or_Hiragana" | "Hrkt" -> Some (Table katakana_or_hiragana)
  | _ -> Option.map (fun t -> Table t) (script_table name)

(* The table of a Script_Extensions value by the long name of the script or an alias. None for any other name. *)
let script_extensions name =
  match name with
  | "Unknown" | "Zzzz" -> Some (Built unknown_script)
  | "Katakana_Or_Hiragana" | "Hrkt" -> Some (Table katakana_or_hiragana)
  | _ -> Option.map (fun t -> Table t) (script_extensions_table name)

(* Maps some of the code points first .. first + length - 1 to themselves plus delta: those whose offset from first has
   none of the bits of mask, so a mask of 1 takes every other code point. A table is four numbers a row. *)
let map_case table r =
  let lo = ref 0 and hi = ref (Array.length table / 4) and result = ref r and fin = ref false in
  while (not !fin) && !lo < !hi do
    let mid = (!lo + !hi) lsr 1 in
    let first = table.(4 * mid) in
    if r < first then hi := mid
    else begin
      let offset = r - first in
      if offset >= table.((4 * mid) + 2) then lo := mid + 1
      else begin
        if offset land table.((4 * mid) + 3) = 0 then result := r + table.((4 * mid) + 1);
        fin := true
      end
    end
  done;
  !result

(* The simple case folding of a code point (the C and S rows of CaseFolding.txt), which is the code point itself when
   it has none. Two code points are equal ignoring case when fold gives the same result for both. *)
let fold r =
  if r < 0x80 then if r >= Char.code 'A' && r <= Char.code 'Z' then r + 32 else r else map_case case_folds r

(* The simple uppercase mapping of a code point, which is the code point itself when it has none. *)
let to_upper r =
  if r < 0x80 then if r >= Char.code 'a' && r <= Char.code 'z' then r - 32 else r else map_case upper_cases r

(* Every code point that fold changes, in ascending order. *)
let folding () =
  let out = Buffer_int.create 2048 in
  for row = 0 to (Array.length case_folds / 4) - 1 do
    let first = case_folds.(4 * row)
    and delta = case_folds.((4 * row) + 1)
    and length = case_folds.((4 * row) + 2)
    and mask = case_folds.((4 * row) + 3) in
    for offset = 0 to length - 1 do
      if offset land mask = 0 && delta <> 0 then Buffer_int.add out (first + offset)
    done
  done;
  Buffer_int.contents out
