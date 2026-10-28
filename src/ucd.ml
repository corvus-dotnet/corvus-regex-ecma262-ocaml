(* The Unicode Character Database properties the engine reads, for each version of Unicode the library carries
   (ucd.rs of the Rust crate): v13, v17 and v18, each a t.

   Nothing in the engine takes Unicode properties from the standard library or the platform, which follow whatever
   built the program. With its own tables the library gives the same answers whatever builds it, and the answers
   change only when the tables are generated again. A pattern is read with the tables of the version it was compiled
   for.

   The tables of a version are in ucd_tables<major>.ml, which tools/unicode/gen-ucd-tables.ps1 writes from the tables
   of the Rust crate, and its properties of strings in strings_tables<major>.ml, so every engine holds the same data.
   Each table is a sorted list of inclusive ranges, the code points up to U+FFFF in r16 and the rest in r32. A value
   that follows from other values (a General_Category group such as L, the Assigned property, the Unknown script) has
   no table of its own and is built when it is first asked for. *)

type table = Ucd_table.t

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

(* What each version's generated modules give. *)
module type TABLES = sig
  val version : string
  val category_table : string -> table option
  val binary_table : string -> table option
  val script_table : string -> table option
  val script_extensions_table : string -> table option
  val case_folds : int array
  val upper_cases : int array
end

(* The Unicode of one version: its lookups by name, its case mappings and its properties of strings, and the values
   built from them on first use and then kept. *)
type t = {
  version : string;
  (* The major number of the version: 13, 17 or 18. *)
  major : int;
  category_table : string -> table option;
  binary_table : string -> table option;
  script_table : string -> table option;
  script_extensions_table : string -> table option;
  case_folds : int array;
  upper_cases : int array;
  (* The properties of strings of the v flag (strings_tables<major>.ml). *)
  strings : (string * string * string) array;
  (* The General_Category values that are groups of other values, the Assigned property (the code points that have a
     General_Category other than Cn) and the Unknown script (the code points that belong to no script: the
     unassigned, private use and surrogate code points). *)
  letter : int array Once.t;
  cased_letter : int array Once.t;
  mark : int array Once.t;
  number : int array Once.t;
  punctuation : int array Once.t;
  symbol : int array Once.t;
  separator : int array Once.t;
  other : int array Once.t;
  assigned : int array Once.t;
  unknown_script : int array Once.t;
}

let make (module T : TABLES) strings =
  let gc names = Once.make (fun () -> union (List.map (fun n -> Option.get (T.category_table n)) names)) in
  {
    version = T.version;
    major = int_of_string (List.hd (String.split_on_char '.' T.version));
    category_table = T.category_table;
    binary_table = T.binary_table;
    script_table = T.script_table;
    script_extensions_table = T.script_extensions_table;
    case_folds = T.case_folds;
    upper_cases = T.upper_cases;
    strings;
    letter = gc [ "Lu"; "Ll"; "Lt"; "Lm"; "Lo" ];
    cased_letter = gc [ "Lu"; "Ll"; "Lt" ];
    mark = gc [ "Mn"; "Mc"; "Me" ];
    number = gc [ "Nd"; "Nl"; "No" ];
    punctuation = gc [ "Pc"; "Pd"; "Ps"; "Pe"; "Pi"; "Pf"; "Po" ];
    symbol = gc [ "Sm"; "Sc"; "Sk"; "So" ];
    separator = gc [ "Zs"; "Zl"; "Zp" ];
    other = gc [ "Cc"; "Cf"; "Cs"; "Co"; "Cn" ];
    assigned = Once.make (fun () -> complement (Option.get (T.category_table "Cn")));
    unknown_script = gc [ "Cn"; "Co"; "Cs" ];
  }

(* The versions of Unicode the library carries. v17 is the default, the version of the V8 the answers are held to. *)
let v13 = make (module Ucd_tables13) Strings_tables13.properties
let v17 = make (module Ucd_tables17) Strings_tables17.properties
let v18 = make (module Ucd_tables18) Strings_tables18.properties

(* The Unicode of a version by its major number: 13, 17 or 18, and the default for any other. A backreference carries
   the number of the version it folds case with, which is never another. *)
let by_major = function 13 -> v13 | 18 -> v18 | _ -> v17

(* per_version f is a function from a version to f's value for it, made for each version now: the way the modules that
   build their own values from a version's tables keep one for each. *)
let per_version f =
  let a = f v13 and b = f v17 and c = f v18 in
  fun u -> match u.major with 13 -> a | 18 -> c | _ -> b

(* The table of the Script value Katakana_Or_Hiragana (Hrkt). PropertyValueAliases.txt lists the value, and no code
   point has it, in Script or in Script_Extensions. Scripts.txt has no line for it, so the generated tables do not
   have it. *)
let katakana_or_hiragana : table = { r16 = [||]; r32 = [||] }

(* The table of a General_Category value by its short name, such as "Lu" or "L". None for any other name. *)
let category u name =
  match name with
  | "L" -> Some (Built u.letter)
  | "LC" -> Some (Built u.cased_letter)
  | "M" -> Some (Built u.mark)
  | "N" -> Some (Built u.number)
  | "P" -> Some (Built u.punctuation)
  | "S" -> Some (Built u.symbol)
  | "Z" -> Some (Built u.separator)
  | "C" -> Some (Built u.other)
  | _ -> Option.map (fun t -> Table t) (u.category_table name)

(* The table of a binary property by its canonical name, such as "Alphabetic" or "White_Space". None for any other
   name. ASCII and Any, which ECMA-262 adds to the properties of Unicode, are not here. *)
let binary u name =
  if name = "Assigned" then Some (Built u.assigned) else Option.map (fun t -> Table t) (u.binary_table name)

(* The table of a Script value by its long name or an alias, such as "Greek" or "Grek". None for any other name. *)
let script u name =
  match name with
  | "Unknown" | "Zzzz" -> Some (Built u.unknown_script)
  | "Katakana_Or_Hiragana" | "Hrkt" -> Some (Table katakana_or_hiragana)
  | _ -> Option.map (fun t -> Table t) (u.script_table name)

(* The table of a Script_Extensions value by the long name of the script or an alias. None for any other name. *)
let script_extensions u name =
  match name with
  | "Unknown" | "Zzzz" -> Some (Built u.unknown_script)
  | "Katakana_Or_Hiragana" | "Hrkt" -> Some (Table katakana_or_hiragana)
  | _ -> Option.map (fun t -> Table t) (u.script_extensions_table name)

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
let fold u r =
  if r < 0x80 then if r >= Char.code 'A' && r <= Char.code 'Z' then r + 32 else r else map_case u.case_folds r

(* The simple uppercase mapping of a code point, which is the code point itself when it has none. *)
let to_upper u r =
  if r < 0x80 then if r >= Char.code 'a' && r <= Char.code 'z' then r - 32 else r else map_case u.upper_cases r

(* Every code point that fold changes, in ascending order. *)
let folding u =
  let case_folds = u.case_folds in
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
