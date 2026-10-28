(* Sets of code points, the classes ECMA-262 defines, and the sets of the Unicode properties (charset.rs of the Rust
   crate). *)

(* The last Unicode code point. *)
let max_rune = Ucd.max_rune

(* An immutable set of code points. The ranges are sorted, inclusive pairs that neither overlap nor touch. ascii
   repeats the membership of the code points below 128, a byte each ('\001' for a member), so that the common case is
   one read; the crate keeps it as two 64-bit words, which an OCaml int cannot hold. nibbles is the ASCII members again
   as the run search of a set reads them (Kernels.ascii_run_end): byte l holds bit h for the member 16 * h + l. *)
type t = { ranges : int array; ascii : string; nibbles : string }

(* Builds a set from inclusive pairs in any order, which may overlap. With negate it builds the complement. *)
let make ?(negate = false) (pairs : int array) =
  let idx = Array.init (Array.length pairs / 2) Fun.id in
  Array.stable_sort (fun a b -> compare pairs.(2 * a) pairs.(2 * b)) idx;
  let merged = Buffer_int.create (Array.length pairs) in
  Array.iter
    (fun i ->
      let lo = pairs.(2 * i) and hi = pairs.((2 * i) + 1) in
      let n = Buffer_int.length merged in
      if n > 0 && lo <= Buffer_int.get merged (n - 1) + 1 then begin
        if hi > Buffer_int.get merged (n - 1) then Buffer_int.set merged (n - 1) hi
      end
      else begin
        Buffer_int.add merged lo;
        Buffer_int.add merged hi
      end)
    idx;
  let ranges =
    if negate then begin
      let m = Buffer_int.contents merged in
      let inverse = Buffer_int.create (Array.length m + 2) in
      let next = ref 0 in
      for k = 0 to (Array.length m / 2) - 1 do
        let lo = m.(2 * k) and hi = m.((2 * k) + 1) in
        if lo > !next then begin
          Buffer_int.add inverse !next;
          Buffer_int.add inverse (lo - 1)
        end;
        next := hi + 1
      done;
      if !next <= max_rune then begin
        Buffer_int.add inverse !next;
        Buffer_int.add inverse max_rune
      end;
      Buffer_int.contents inverse
    end
    else Buffer_int.contents merged
  in
  let ascii = Bytes.make 128 '\000' in
  (try
     for k = 0 to (Array.length ranges / 2) - 1 do
       let lo = ranges.(2 * k) and hi = ranges.((2 * k) + 1) in
       if lo >= 128 then raise Exit;
       for c = lo to min hi 127 do
         Bytes.set ascii c '\001'
       done
     done
   with Exit -> ());
  let nibbles = Bytes.make 16 '\000' in
  for c = 0 to 127 do
    if Bytes.get ascii c <> '\000' then
      Bytes.set nibbles (c land 15) (Char.chr (Char.code (Bytes.get nibbles (c land 15)) lor (1 lsl (c lsr 4))))
  done;
  { ranges; ascii = Bytes.unsafe_to_string ascii; nibbles = Bytes.unsafe_to_string nibbles }

(* Whether the set holds the ASCII character (c below 128). *)
let has_ascii s c = String.unsafe_get s.ascii (c land 127) <> '\000'

(* Whether the set holds the code point. *)
let contains s r =
  if r >= 0 && r < 128 then String.unsafe_get s.ascii r <> '\000'
  else begin
    let ranges = s.ranges in
    let lo = ref 0 and hi = ref (Array.length ranges / 2) and found = ref false in
    while (not !found) && !lo < !hi do
      let mid = (!lo + !hi) lsr 1 in
      if r > ranges.((2 * mid) + 1) then lo := mid + 1
      else if r < ranges.(2 * mid) then hi := mid
      else found := true
    done;
    !found
  end

(* Whether the set holds no code point. *)
let is_empty s = Array.length s.ranges = 0

(* The classes ECMA-262 defines. \d and \w are ASCII only, \s is WhiteSpace and LineTerminator, and . is everything but
   the four line terminators. *)
let digit_pairs = [| 0x30; 0x39 |]
let word_pairs = [| 0x30; 0x39; 0x41; 0x5A; 0x5F; 0x5F; 0x61; 0x7A |]

let space_pairs =
  [|
    0x9; 0xD; 0x20; 0x20; 0xA0; 0xA0; 0x1680; 0x1680; 0x2000; 0x200A; 0x2028; 0x2029; 0x202F; 0x202F; 0x205F; 0x205F;
    0x3000; 0x3000; 0xFEFF; 0xFEFF;
  |]

let line_terminator_pairs = [| 0xA; 0xA; 0xD; 0xD; 0x2028; 0x2029 |]
let dot_set = make ~negate:true line_terminator_pairs

(* Whether a byte is an ECMA-262 word character. Every word character is ASCII, so a word boundary can be decided from
   the units on either side of a position. *)
let is_word_byte b =
  (b >= Char.code 'a' && b <= Char.code 'z')
  || (b >= Char.code 'A' && b <= Char.code 'Z')
  || (b >= Char.code '0' && b <= Char.code '9')
  || b = Char.code '_'

(* The short name of a General_Category value, from its long name, an alias or the short name itself. *)
let category_name = function
  | "Letter" | "L" -> Some "L"
  | "Lowercase_Letter" | "Ll" -> Some "Ll"
  | "Uppercase_Letter" | "Lu" -> Some "Lu"
  | "Titlecase_Letter" | "Lt" -> Some "Lt"
  | "Cased_Letter" | "LC" -> Some "LC"
  | "Modifier_Letter" | "Lm" -> Some "Lm"
  | "Other_Letter" | "Lo" -> Some "Lo"
  | "Mark" | "Combining_Mark" | "M" -> Some "M"
  | "Nonspacing_Mark" | "Mn" -> Some "Mn"
  | "Spacing_Mark" | "Mc" -> Some "Mc"
  | "Enclosing_Mark" | "Me" -> Some "Me"
  | "Number" | "N" -> Some "N"
  | "Decimal_Number" | "digit" | "Nd" -> Some "Nd"
  | "Letter_Number" | "Nl" -> Some "Nl"
  | "Other_Number" | "No" -> Some "No"
  | "Punctuation" | "punct" | "P" -> Some "P"
  | "Connector_Punctuation" | "Pc" -> Some "Pc"
  | "Dash_Punctuation" | "Pd" -> Some "Pd"
  | "Open_Punctuation" | "Ps" -> Some "Ps"
  | "Close_Punctuation" | "Pe" -> Some "Pe"
  | "Initial_Punctuation" | "Pi" -> Some "Pi"
  | "Final_Punctuation" | "Pf" -> Some "Pf"
  | "Other_Punctuation" | "Po" -> Some "Po"
  | "Symbol" | "S" -> Some "S"
  | "Math_Symbol" | "Sm" -> Some "Sm"
  | "Currency_Symbol" | "Sc" -> Some "Sc"
  | "Modifier_Symbol" | "Sk" -> Some "Sk"
  | "Other_Symbol" | "So" -> Some "So"
  | "Separator" | "Z" -> Some "Z"
  | "Space_Separator" | "Zs" -> Some "Zs"
  | "Line_Separator" | "Zl" -> Some "Zl"
  | "Paragraph_Separator" | "Zp" -> Some "Zp"
  | "Other" | "C" -> Some "C"
  | "Control" | "cntrl" | "Cc" -> Some "Cc"
  | "Format" | "Cf" -> Some "Cf"
  | "Surrogate" | "Cs" -> Some "Cs"
  | "Private_Use" | "Co" -> Some "Co"
  | "Unassigned" | "Cn" -> Some "Cn"
  | _ -> None

(* The canonical name of a binary property ECMA-262 lists, from the name or an alias. *)
let binary_name = function
  | "ASCII" -> Some "ASCII"
  | "ASCII_Hex_Digit" | "AHex" -> Some "ASCII_Hex_Digit"
  | "Alphabetic" | "Alpha" -> Some "Alphabetic"
  | "Any" -> Some "Any"
  | "Assigned" -> Some "Assigned"
  | "Bidi_Control" | "Bidi_C" -> Some "Bidi_Control"
  | "Bidi_Mirrored" | "Bidi_M" -> Some "Bidi_Mirrored"
  | "Case_Ignorable" | "CI" -> Some "Case_Ignorable"
  | "Cased" -> Some "Cased"
  | "Changes_When_Casefolded" | "CWCF" -> Some "Changes_When_Casefolded"
  | "Changes_When_Casemapped" | "CWCM" -> Some "Changes_When_Casemapped"
  | "Changes_When_Lowercased" | "CWL" -> Some "Changes_When_Lowercased"
  | "Changes_When_NFKC_Casefolded" | "CWKCF" -> Some "Changes_When_NFKC_Casefolded"
  | "Changes_When_Titlecased" | "CWT" -> Some "Changes_When_Titlecased"
  | "Changes_When_Uppercased" | "CWU" -> Some "Changes_When_Uppercased"
  | "Dash" -> Some "Dash"
  | "Default_Ignorable_Code_Point" | "DI" -> Some "Default_Ignorable_Code_Point"
  | "Deprecated" | "Dep" -> Some "Deprecated"
  | "Diacritic" | "Dia" -> Some "Diacritic"
  | "Emoji" -> Some "Emoji"
  | "Emoji_Component" | "EComp" -> Some "Emoji_Component"
  | "Emoji_Modifier" | "EMod" -> Some "Emoji_Modifier"
  | "Emoji_Modifier_Base" | "EBase" -> Some "Emoji_Modifier_Base"
  | "Emoji_Presentation" | "EPres" -> Some "Emoji_Presentation"
  | "Extended_Pictographic" | "ExtPict" -> Some "Extended_Pictographic"
  | "Extender" | "Ext" -> Some "Extender"
  | "Grapheme_Base" | "Gr_Base" -> Some "Grapheme_Base"
  | "Grapheme_Extend" | "Gr_Ext" -> Some "Grapheme_Extend"
  | "Hex_Digit" | "Hex" -> Some "Hex_Digit"
  | "IDS_Binary_Operator" | "IDSB" -> Some "IDS_Binary_Operator"
  | "IDS_Trinary_Operator" | "IDST" -> Some "IDS_Trinary_Operator"
  | "ID_Continue" | "IDC" -> Some "ID_Continue"
  | "ID_Start" | "IDS" -> Some "ID_Start"
  | "Ideographic" | "Ideo" -> Some "Ideographic"
  | "Join_Control" | "Join_C" -> Some "Join_Control"
  | "Logical_Order_Exception" | "LOE" -> Some "Logical_Order_Exception"
  | "Lowercase" | "Lower" -> Some "Lowercase"
  | "Math" -> Some "Math"
  | "Noncharacter_Code_Point" | "NChar" -> Some "Noncharacter_Code_Point"
  | "Pattern_Syntax" | "Pat_Syn" -> Some "Pattern_Syntax"
  | "Pattern_White_Space" | "Pat_WS" -> Some "Pattern_White_Space"
  | "Quotation_Mark" | "QMark" -> Some "Quotation_Mark"
  | "Radical" -> Some "Radical"
  | "Regional_Indicator" | "RI" -> Some "Regional_Indicator"
  | "Sentence_Terminal" | "STerm" -> Some "Sentence_Terminal"
  | "Soft_Dotted" | "SD" -> Some "Soft_Dotted"
  | "Terminal_Punctuation" | "Term" -> Some "Terminal_Punctuation"
  | "Unified_Ideograph" | "UIdeo" -> Some "Unified_Ideograph"
  | "Uppercase" | "Upper" -> Some "Uppercase"
  | "Variation_Selector" | "VS" -> Some "Variation_Selector"
  | "White_Space" | "space" -> Some "White_Space"
  | "XID_Continue" | "XIDC" -> Some "XID_Continue"
  | "XID_Start" | "XIDS" -> Some "XID_Start"
  | _ -> None

let category_pairs u value =
  match category_name value with None -> None | Some n -> Option.map Ucd.pairs (Ucd.category u n)

(* The ranges of a property expression under a version of Unicode. Every range comes from the tables of Ucd of the
   version, so the answer does not depend on what built the program. *)
let property_pairs u expr =
  match String.index_opt expr '=' with
  | Some k -> (
      let name = String.sub expr 0 k and value = String.sub expr (k + 1) (String.length expr - k - 1) in
      match name with
      | "General_Category" | "gc" -> category_pairs u value
      | "Script" | "sc" -> Option.map Ucd.pairs (Ucd.script u value)
      | "Script_Extensions" | "scx" -> Option.map Ucd.pairs (Ucd.script_extensions u value)
      | _ -> None)
  | None -> (
      match category_pairs u expr with
      | Some p -> Some p
      | None -> (
          match binary_name expr with
          | None -> None
          | Some "ASCII" -> Some [| 0; 0x7F |]
          | Some "Any" -> Some [| 0; max_rune |]
          | Some canonical -> Option.map Ucd.pairs (Ucd.binary u canonical)))

module Smap = Map.Make (String)

(* The sets already built for property expressions, one cache for each version of Unicode. A property set can have
   hundreds of ranges, and patterns tend to repeat the same few. Only an expression ECMA-262 defines is kept, and those
   are a fixed few hundred, so each cache is bounded. The map is immutable and replaced with a compare and set, so the
   cache needs no lock on OCaml 4.14 or 5. *)
let property_cache : Ucd.t -> t Smap.t Atomic.t = Ucd.per_version (fun _ -> Atomic.make Smap.empty)

(* Resolves the expression between the braces of \p{...} to its set under a version of Unicode. None for a name or
   value that ECMA-262 does not define. *)
let property_set u expr =
  let property_cache = property_cache u in
  match Smap.find_opt expr (Atomic.get property_cache) with
  | Some s -> Some s
  | None -> (
      match property_pairs u expr with
      | None -> None
      | Some pairs ->
          let set = make pairs in
          let rec store () =
            let m = Atomic.get property_cache in
            if not (Atomic.compare_and_set property_cache m (Smap.add expr set m)) then store ()
          in
          store ();
          Some set)

(* The code points in both sets. *)
let intersect a b =
  let ra = a.ranges and rb = b.ranges in
  let na = Array.length ra / 2 and nb = Array.length rb / 2 in
  let i = ref 0 and j = ref 0 in
  let pairs = Buffer_int.create 16 in
  while !i < na && !j < nb do
    let lo = max ra.(2 * !i) rb.(2 * !j) and hi = min ra.((2 * !i) + 1) rb.((2 * !j) + 1) in
    if lo <= hi then begin
      Buffer_int.add pairs lo;
      Buffer_int.add pairs hi
    end;
    if ra.((2 * !i) + 1) < rb.((2 * !j) + 1) then incr i else incr j
  done;
  make (Buffer_int.contents pairs)

(* The code points not in the set. *)
let complement s = make ~negate:true s.ranges

(* The code points of the first set that are not in the second. *)
let subtract a b = intersect a (complement b)

(* The code points of either set. *)
let union a b = make (Array.append a.ranges b.ranges)

(* The value of a property of strings, which only the v flag can name: its single code points and its sequences. *)
type string_property = { set : t; strings : int array list }

let hex s = int_of_string ("0x" ^ s)

(* The properties of strings of a version of Unicode, read from its strings on first use and kept with it, the union of
   them all last. *)
let string_properties =
  Ucd.per_version (fun (u : Ucd.t) ->
  Once.make (fun () ->
      let all = ref [] and union_pairs = ref [] and every = ref [] in
      Array.iter
        (fun (_, code_points, sequences) ->
          let pairs = Buffer_int.create 64 in
          List.iter
            (fun item ->
              if item <> "" then begin
                let lo, hi =
                  match String.index_opt item '.' with
                  | Some k -> (String.sub item 0 k, String.sub item (k + 2) (String.length item - k - 2))
                  | None -> (item, item)
                in
                Buffer_int.add pairs (hex lo);
                Buffer_int.add pairs (hex hi)
              end)
            (String.split_on_char ' ' code_points);
          let strings =
            List.filter_map
              (fun s ->
                if s = "" then None else Some (Array.of_list (List.map hex (String.split_on_char ' ' s))))
              (String.split_on_char ';' sequences)
          in
          let p = Buffer_int.contents pairs in
          union_pairs := p :: !union_pairs;
          every := !every @ strings;
          all := { set = make p; strings } :: !all)
        u.strings;
      let rgi = { set = make (Array.concat (List.rev !union_pairs)); strings = !every } in
      Array.of_list (List.rev (rgi :: !all))))

(* The property of strings of the name (RGI_Emoji and the six it is the union of) under a version of Unicode, or
   None. *)
let string_property (u : Ucd.t) name =
  let props = Once.get (string_properties u) in
  if name = "RGI_Emoji" then Some props.(Array.length props - 1)
  else begin
    let found = ref None in
    Array.iteri (fun k (n, _, _) -> if n = name then found := Some props.(k)) u.strings;
    !found
  end
