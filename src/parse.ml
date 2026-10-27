(* The parser: a pattern to its syntax tree, in either grammar, with the limits of size (parse.rs of the Rust crate). *)

type kind =
  | Empty
  | Char
  | Set
  | Cat
  | Alt
  | Group
  | Repeat
  | Bol
  | Eol
  (* Line_start and Line_end are ^ and $ with the m flag or modifier. *)
  | Line_start
  | Line_end
  | Word_boundary
  | Not_word_boundary
  | Look
  | Backref

(* The max of a quantifier with no upper bound. *)
let unbounded = -1

(* Caps the bounds of a counted quantifier. No text that fits in memory reaches a larger count. *)
let max_count = 0x7FFFFFFF

(* The limits on the size of one pattern. A pattern may come from outside, so reading and compiling one must take
   bounded time and memory whatever it holds. A pattern over a limit is refused with Too_large, which is not the error
   of a pattern that is not valid. *)

(* The longest pattern that is read, in bytes of UTF-8. *)
let max_pattern_length = 1 lsl 20

(* Caps the depth of groups, which bounds the recursion of the parser and of the compilers. *)
let max_nesting = 1000

(* The largest size a pattern may have, in the bytes the parser counts for it: node_size for each node of its syntax
   tree, and range_size for each range of a class as its members are read, for each range a case-insensitive group or
   a negated property adds, for each alternative that encloses a named group and for each group that shares its name.
   The tree, the program compiled from it and the time to make both are proportional to that size, so it bounds them
   all. *)
let max_size = 1 lsl 20

let node_size = 32
let range_size = 8

(* Which limit of size a pattern is over. *)
type limit = Length | Nesting | Size

let limit_name = function Length -> "length" | Nesting -> "nesting" | Size -> "size"

let too_large_message limit value =
  match limit with
  | Length -> Printf.sprintf "the pattern is longer than %d bytes" value
  | Nesting -> Printf.sprintf "the groups of the pattern nest more than %d deep" value
  | Size -> Printf.sprintf "the pattern has a size of more than %d bytes when it is read" value

(* Why a pattern was not read: it is over a limit of size, or it is not valid under the chosen grammar (the reason,
   and the offset in the pattern's characters). *)
exception Too_large of limit * int

exception Syntax of string * int

(* One element of the syntax tree of a pattern. *)
type node = {
  kind : kind;
  (* The code point of a Char. *)
  r : int;
  (* The class of a Set. *)
  set : Charset.t option;
  (* The children of Cat and Alt, and the one child of Group, Repeat and Look. *)
  subs : node array;
  (* min, max and lazy_ describe a Repeat. *)
  min : int;
  max : int;
  lazy_ : bool;
  (* The number of the capturing group of a Group, counted from 1. *)
  group : int;
  (* The groups a Backref names. A name can belong to several groups in separate alternatives, of which at most one has
     taken part at any time. *)
  groups : int array;
  (* The group name of a named backreference until it is resolved. *)
  name : int array;
  (* Marks a Backref, Word_boundary or Not_word_boundary inside a case-insensitive group. *)
  fold : bool;
  (* behind and negative describe a Look. *)
  behind : bool;
  negative : bool;
}

let node kind =
  {
    kind;
    r = 0;
    set = None;
    subs = [||];
    min = 0;
    max = 0;
    lazy_ = false;
    group = 0;
    groups = [||];
    name = [||];
    fold = false;
    behind = false;
    negative = false;
  }

let with_subs kind subs = { (node kind) with subs }
let char_of r = { (node Char) with r }
let set_of s = { (node Set) with set = Some s }

(* The class of a Set. *)
let class_of n = match n.set with Some s -> s | None -> invalid_arg "a set node has a class"

(* Names one alternative of one disjunction. *)
type branch = { disjunction : int; mutable alternative : int }

(* A capturing group with a name, and the alternatives that enclose it. *)
type named_group = { ngroup : int; npath : branch array }

(* The grammar a pattern is read with and the flags that change what its atoms mean. *)
type syntax = {
  (* The grammar of the u flag. Without it, the Annex B grammar of a pattern with no u or v flag. *)
  unicode : bool;
  (* The grammar of the v flag inside a class, and the properties of strings. It comes with unicode. *)
  sets : bool;
  (* The i, m and s flags: the modifiers in force at the start of the pattern. *)
  icase : bool;
  multiline : bool;
  dot_all : bool;
}

let default_syntax = { unicode = false; sets = false; icase = false; multiline = false; dot_all = false }

(* Reads a pattern by recursive descent. With unicode set it applies the grammar of the u flag. Without it, it applies
   the Annex B grammar of patterns with no flag. *)
type parser = {
  (* The pattern as code points with the u flag, and as UTF-16 code units without it (see code_units). *)
  p : int array;
  mutable i : int;
  unicode : bool;
  (* The grammar of the v flag. *)
  sets : bool;
  (* The sequences of the property of strings the last class escape named (the v flag), or None. *)
  mutable escape_strings : int array list option;
  mutable depth : int;
  (* group_count and has_names come from a scan of the whole pattern before parsing, because the meaning of \1 and \k
     depends on groups that may follow them. *)
  mutable group_count : int;
  mutable has_names : bool;
  mutable groups : int;
  names : (int array, named_group list) Hashtbl.t;
  (* The order in which names were first seen. *)
  mutable name_order : int array list;
  mutable named_refs : bool;
  (* The alternatives that enclose the current position, innermost last. *)
  mutable path : branch list;
  mutable disjunctions : int;
  (* The modifiers in force, which only a group such as (?i:...) changes. *)
  mutable icase : bool;
  mutable multiline : bool;
  mutable dot_all : bool;
  (* Records that the pattern has a construct only the backtracking matcher can run. *)
  mutable needs_backtracking : bool;
  (* The size of what has been read so far (see max_size). *)
  mutable size : int;
}

let any_set = Charset.make [| 0; Charset.max_rune |]
let digit_set = Charset.make Charset.digit_pairs
let not_digit_set = Charset.make ~negate:true Charset.digit_pairs
let word_set = Charset.make Charset.word_pairs
let not_word_set = Charset.make ~negate:true Charset.word_pairs
let space_set = Charset.make Charset.space_pairs
let not_space_set = Charset.make ~negate:true Charset.space_pairs

(* The code points of a UTF-8 pattern, which the caller has checked is UTF-8. *)
let code_points pattern =
  let out = Buffer_int.create (String.length pattern) in
  let n = String.length pattern in
  let at = ref 0 in
  while !at < n do
    let p = Units.decode_bytes pattern !at n in
    Buffer_int.add out (Units.rune_of p);
    at := !at + Units.width_of p
  done;
  Buffer_int.contents out

(* The UTF-16 code units of a pattern. Without the u flag ECMA-262 reads a pattern, and matches a text, a code unit at
   a time. A character beyond the Basic Multilingual Plane is then two atoms, a quantifier after it repeats the
   second, and in a class it is two members, so a range between two such characters runs from a low surrogate to a
   high one and is an error. *)
let code_units p =
  if not (Array.exists (fun c -> c > 0xFFFF) p) then p
  else begin
    let out = Buffer_int.create (Array.length p + 8) in
    Array.iter
      (fun c ->
        if c > 0xFFFF then begin
          Buffer_int.add out (Units.high_surrogate c);
          Buffer_int.add out (Units.low_surrogate c)
        end
        else Buffer_int.add out c)
      p;
    Buffer_int.contents out
  end

let ch c = Char.code c
let is_digit c = c >= ch '0' && c <= ch '9'

(* Whether the first of two runs of decimal digits is the greater number. The runs are compared as written, because a
   count has no limit of size in the grammar and number stops at max_count. *)
let greater_as_written p a0 a1 b0 b1 =
  let a0 = ref a0 and b0 = ref b0 in
  while !a0 < a1 && p.(!a0) = ch '0' do
    incr a0
  done;
  while !b0 < b1 && p.(!b0) = ch '0' do
    incr b0
  done;
  let la = a1 - !a0 and lb = b1 - !b0 in
  if la <> lb then la > lb
  else begin
    let result = ref false and fin = ref false and k = ref 0 in
    while (not !fin) && !k < la do
      let x = p.(!a0 + !k) and y = p.(!b0 + !k) in
      if x <> y then begin
        result := x > y;
        fin := true
      end;
      incr k
    done;
    !result
  end

(* Whether two positions lie in different alternatives of some disjunction. *)
let separate_alternatives (a : branch array) (b : branch array) =
  let n = min (Array.length a) (Array.length b) in
  let result = ref false and fin = ref false and k = ref 0 in
  while (not !fin) && !k < n do
    let x = a.(!k) and y = b.(!k) in
    if x.disjunction <> y.disjunction then fin := true
    else if x.alternative <> y.alternative then begin
      result := true;
      fin := true
    end;
    incr k
  done;
  !result

let hex_value c =
  if c >= 0x30 && c <= 0x39 then c - 0x30
  else if c >= 0x61 && c <= 0x66 then c - 0x61 + 10
  else if c >= 0x41 && c <= 0x46 then c - 0x41 + 10
  else -1

let is_ascii_letter c = (c >= ch 'a' && c <= ch 'z') || (c >= ch 'A' && c <= ch 'Z')
let in_ascii c chars = c >= 0 && c < 128 && String.contains chars (Char.chr c)

(* Whether c is one of the characters the u flag lets an identity escape name. *)
let syntax_character c = in_ascii c "^$\\.*+?()[]{}|/"

let id_start = match Ucd_tables.binary_table "ID_Start" with Some t -> t | None -> failwith "no ID_Start"
let id_continue = match Ucd_tables.binary_table "ID_Continue" with Some t -> t | None -> failwith "no ID_Continue"

let append_class_atom pairs c set =
  match set with
  | Some (s : Charset.t) -> Buffer_int.append_array pairs s.ranges
  | None ->
      Buffer_int.add pairs c;
      Buffer_int.add pairs c

(* The number of capturing groups of the pattern. *)
let group_count ps = ps.groups

(* The names of the groups, each with the groups that have it, in the order of their first group. *)
let group_names ps =
  let named =
    List.map
      (fun name ->
        let gs = List.rev (Hashtbl.find ps.names name) in
        (name, Array.of_list (List.map (fun g -> g.ngroup) gs)))
      (List.rev ps.name_order)
  in
  List.stable_sort (fun (_, a) (_, b) -> compare a.(0) b.(0)) named

(* Adds to the size of the pattern, and stops the parser when the size is over its limit. *)
let charge ps bytes =
  ps.size <- ps.size + bytes;
  if ps.size > max_size then raise (Too_large (Size, max_size))

let fail ps msg = raise (Syntax (msg, ps.i))

(* Gives every named backreference the groups of its name. A name no group has is an error. *)
let rec resolve ps n =
  let n =
    if n.kind = Backref && Array.length n.name > 0 then
      match Hashtbl.find_opt ps.names n.name with
      | None -> fail ps "reference to an unknown group name"
      | Some named ->
          { n with groups = Array.append n.groups (Array.of_list (List.rev_map (fun g -> g.ngroup) named)) }
    else n
  in
  if Array.length n.subs = 0 then n else { n with subs = Array.map (resolve ps) n.subs }

(* Counts the capturing groups of the pattern and notes whether any is named. *)
let scan_groups ps =
  let in_class = ref false and s = ps.p and k = ref 0 in
  let n = Array.length s in
  while !k < n do
    let c = s.(!k) in
    if c = ch '\\' then incr k
    else if c = ch '[' then in_class := true
    else if c = ch ']' then in_class := false
    else if c = ch '(' && not !in_class then begin
      if !k + 1 >= n || s.(!k + 1) <> ch '?' then ps.group_count <- ps.group_count + 1
      else if !k + 3 < n && s.(!k + 2) = ch '<' && s.(!k + 3) <> ch '=' && s.(!k + 3) <> ch '!' then begin
        ps.group_count <- ps.group_count + 1;
        ps.has_names <- true
      end
    end;
    incr k
  done

let more ps = ps.i < Array.length ps.p
let peek ps = if ps.i < Array.length ps.p then ps.p.(ps.i) else -1

let eat ps c =
  if ps.i < Array.length ps.p && ps.p.(ps.i) = ch c then begin
    ps.i <- ps.i + 1;
    true
  end
  else false

(* The set with every character equivalent to one of its members. It charges the pattern for the work and for the set:
   a byte for each character with a case variant that the set holds, and its ranges. *)
let fold_set ps set =
  let closed, variants = Fold.fold_closure set ps.unicode in
  charge ps (variants + (range_size * Array.length closed.Charset.ranges / 2));
  closed

(* The set with every character equivalent to one of its members, inside a case-insensitive group, and the set itself
   outside one. *)
let fold_if ps set = if ps.icase then fold_set ps set else set

(* The node for one literal character. Inside a case-insensitive group that is the class of the characters equivalent
   to it. *)
let char_node ps c =
  if ps.icase then begin
    let set, _ = Fold.fold_closure (Charset.make [| c; c |]) ps.unicode in
    if Array.length set.ranges <> 2 || set.ranges.(0) <> set.ranges.(1) then set_of set else char_of c
  end
  else char_of c

(* The node for a class. Inside a case-insensitive group the class also takes every character equivalent to one of its
   members. *)
let set_node ps set = set_of (if ps.icase then fold_set ps set else set)

(* Whether the text at the current { reads as {n}, {n,} or {n,m}. *)
let looks_like_quantifier ps =
  let s = ps.p and n = Array.length ps.p in
  let k = ref (ps.i + 1) in
  let start = !k in
  while !k < n && is_digit s.(!k) do
    incr k
  done;
  if !k = start || !k >= n then false
  else if s.(!k) = ch '}' then true
  else if s.(!k) <> ch ',' then false
  else begin
    incr k;
    while !k < n && is_digit s.(!k) do
      incr k
    done;
    !k < n && s.(!k) = ch '}'
  end

(* Reads a run of decimal digits, saturating at max_count. *)
let number ps =
  let start = ps.i in
  let v = ref 0 in
  while ps.i < Array.length ps.p && is_digit ps.p.(ps.i) do
    if !v < max_count then begin
      v := (!v * 10) + (ps.p.(ps.i) - ch '0');
      if !v > max_count then v := max_count
    end;
    ps.i <- ps.i + 1
  done;
  if start = ps.i then fail ps "expected a number";
  !v

(* Reads a quantifier, when one is here: its bounds, and whether it is lazy. *)
let quantifier ps =
  let c = peek ps in
  let bounds =
    if c = ch '*' then begin
      ps.i <- ps.i + 1;
      Some (0, unbounded)
    end
    else if c = ch '+' then begin
      ps.i <- ps.i + 1;
      Some (1, unbounded)
    end
    else if c = ch '?' then begin
      ps.i <- ps.i + 1;
      Some (0, 1)
    end
    else if c = ch '{' && looks_like_quantifier ps then begin
      ps.i <- ps.i + 1;
      let first = ps.i in
      let lo = number ps in
      let hi = ref lo in
      let first_end = ps.i in
      if eat ps ',' then begin
        if peek ps = ch '}' then hi := unbounded
        else begin
          let second = ps.i in
          hi := number ps;
          (* Both counts stop at max_count, so the numbers are compared as they are written. *)
          if greater_as_written ps.p first first_end second ps.i then fail ps "numbers out of order in quantifier"
        end
      end;
      if not (eat ps '}') then fail ps "malformed quantifier";
      Some (lo, !hi)
    end
    else None
  in
  match bounds with None -> None | Some (min, max) -> Some (min, max, eat ps '?')

let close ps = if not (eat ps ')') then fail ps "unterminated group"

(* Reads exactly n hexadecimal digits. It returns -1, consuming nothing, when they are not there. *)
let hex ps n =
  if ps.i + n > Array.length ps.p then -1
  else begin
    let v = ref 0 and bad = ref false in
    for k = 0 to n - 1 do
      let d = hex_value ps.p.(ps.i + k) in
      if d < 0 then bad := true else v := (!v * 16) + d
    done;
    if !bad then -1
    else begin
      ps.i <- ps.i + n;
      !v
    end
  end

(* Reads what follows a \u and returns the code point, or -1, consuming nothing, when it is not a well-formed escape.
   With unicode_mode set, as it is with the u flag and always in a group name, it accepts the \u{...} form and reads a
   surrogate pair written as two escapes as one code point. Without it each escape is one code unit. *)
let unicode_escape ps unicode_mode =
  let start = ps.i in
  if unicode_mode && eat ps '{' then begin
    let digits = ps.i in
    let v = ref 0 in
    while more ps && hex_value (peek ps) >= 0 do
      if !v <= Charset.max_rune then v := (!v * 16) + hex_value (peek ps);
      ps.i <- ps.i + 1
    done;
    if digits = ps.i || (not (eat ps '}')) || !v > Charset.max_rune then begin
      ps.i <- start;
      -1
    end
    else !v
  end
  else begin
    let u = hex ps 4 in
    if u < 0 then -1
    else if
      unicode_mode
      && u >= 0xD800
      && u <= 0xDBFF
      && ps.i + 6 <= Array.length ps.p
      && ps.p.(ps.i) = ch '\\'
      && ps.p.(ps.i + 1) = ch 'u'
    then begin
      let save = ps.i in
      ps.i <- ps.i + 2;
      let low = hex ps 4 in
      if low >= 0xDC00 && low <= 0xDFFF then 0x10000 + ((u - 0xD800) lsl 10) + (low - 0xDC00)
      else begin
        ps.i <- save;
        u
      end
    end
    else u
  end

(* Reads a group name up to and including its >. A character of the name may be written as a \u escape. *)
let group_name ps =
  let name = Buffer_int.create 8 in
  while not (eat ps '>') do
    if not (more ps) then fail ps "invalid group name";
    let c = ref (peek ps) in
    ps.i <- ps.i + 1;
    if !c = ch '\\' then begin
      if not (eat ps 'u') then fail ps "invalid group name";
      c := unicode_escape ps true;
      if !c < 0 then fail ps "invalid group name"
    end
    else if !c >= 0xD800 && !c <= 0xDBFF && peek ps >= 0xDC00 && peek ps <= 0xDFFF then begin
      (* Without the u flag the pattern is code units, and a name still takes a surrogate pair as one character. *)
      c := 0x10000 + ((!c - 0xD800) lsl 10) + (peek ps - 0xDC00);
      ps.i <- ps.i + 1
    end;
    if Buffer_int.length name = 0 then begin
      if not (!c = ch '$' || !c = ch '_' || Ucd.contains id_start !c) then fail ps "invalid group name"
    end
    else if not (!c = ch '$' || !c = 0x200C || !c = 0x200D || Ucd.contains id_continue !c) then
      fail ps "invalid group name";
    Buffer_int.add name !c
  done;
  if Buffer_int.length name = 0 then fail ps "invalid group name";
  Buffer_int.contents name

(* Reads an Annex B octal escape of up to three digits with a value below 256. A digit that cannot start one (8 or 9)
   stands for itself. *)
let legacy_octal ps =
  let v = ref 0 and k = ref 0 in
  while
    !k < 3 && more ps && peek ps >= ch '0' && peek ps <= ch '7' && (!v * 8) + (peek ps - ch '0') <= 0o377
  do
    v := (!v * 8) + (peek ps - ch '0');
    ps.i <- ps.i + 1;
    incr k
  done;
  if !k = 0 then begin
    let d = peek ps in
    ps.i <- ps.i + 1;
    d
  end
  else !v

(* Reads a class escape such as \d or \p{...} whose letter is c. It returns None, consuming nothing, when c does not
   start one. *)
let class_escape ps c =
  ps.escape_strings <- None;
  let simple set =
    ps.i <- ps.i + 1;
    Some set
  in
  if c = ch 'd' then simple digit_set
  else if c = ch 'D' then simple not_digit_set
  else if c = ch 'w' then simple (if ps.unicode && ps.icase then Fold.fold_word_set else word_set)
  else if c = ch 'W' then simple (if ps.unicode && ps.icase then Fold.not_fold_word_set else not_word_set)
  else if c = ch 's' then simple space_set
  else if c = ch 'S' then simple not_space_set
  else if c = ch 'p' || c = ch 'P' then begin
    (* Without the u flag \p is the letter p. *)
    if not ps.unicode then None
    else begin
      ps.i <- ps.i + 1;
      if not (eat ps '{') then fail ps "invalid property name";
      let start = ps.i in
      while more ps && peek ps <> ch '}' do
        ps.i <- ps.i + 1
      done;
      (* A name ECMA-262 defines is ASCII. Any other character makes a name no table has. *)
      let expr = String.init (ps.i - start) (fun k -> let r = ps.p.(start + k) in if r < 128 then Char.chr r else '\x7F') in
      if not (eat ps '}') then fail ps "invalid property name";
      match Charset.property_set expr with
      | None -> (
          match if ps.sets && c = ch 'p' then Charset.string_property expr else None with
          | Some property ->
              (* A property of strings, which the v flag alone names, and which has no complement. *)
              charge ps (range_size * ((Array.length property.set.ranges / 2) + List.length property.strings));
              ps.escape_strings <- Some property.strings;
              Some property.set
          | None -> fail ps "invalid property name")
      | Some set ->
          if c = ch 'P' then begin
            charge ps (range_size * Array.length set.ranges / 2);
            (* With the v flag the complement is of the case-folded set; with the u flag the fold comes after it. *)
            let set = if ps.sets then fold_if ps set else set in
            Some (Charset.complement set)
          end
          else Some set
    end
  end
  else None

(* Reads the escape after a backslash, in or out of a class, and returns the code point it names. *)
let character_escape ps in_class =
  let c = peek ps in
  ps.i <- ps.i + 1;
  if c = ch 'f' then 0x0C
  else if c = ch 'n' then 0x0A
  else if c = ch 'r' then 0x0D
  else if c = ch 't' then 0x09
  else if c = ch 'v' then 0x0B
  else if c = ch 'c' then begin
    let l = peek ps in
    if is_ascii_letter l then begin
      ps.i <- ps.i + 1;
      l mod 32
    end
    else if (not ps.unicode) && in_class && (is_digit l || l = ch '_') then begin
      ps.i <- ps.i + 1;
      l mod 32
    end
    else if ps.unicode then fail ps "invalid control escape"
    else begin
      (* Annex B reads a \c that is not a control escape as a backslash followed by the letter c. *)
      ps.i <- ps.i - 1;
      ch '\\'
    end
  end
  else if c = ch '0' then begin
    if more ps && is_digit (peek ps) then begin
      if ps.unicode then fail ps "invalid decimal escape";
      ps.i <- ps.i - 1;
      legacy_octal ps
    end
    else 0
  end
  else if c = ch 'x' then begin
    let h = hex ps 2 in
    if h < 0 then if ps.unicode then fail ps "invalid \\x escape" else ch 'x' else h
  end
  else if c = ch 'u' then begin
    let u = unicode_escape ps ps.unicode in
    if u < 0 then if ps.unicode then fail ps "invalid \\u escape" else ch 'u' else u
  end
  else if c >= ch '1' && c <= ch '9' && (not ps.unicode) && in_class then begin
    ps.i <- ps.i - 1;
    legacy_octal ps
  end
  else begin
    (* Identity escapes. The u flag allows only the syntax characters and '/', and '-' in a class. Annex B allows any
       character, except k in a pattern with a named group. Outside a class atom_escape reads \k before this is
       reached, so here the \k of such a pattern is in a class, where it is not a reference either. *)
    if ps.unicode && (not (syntax_character c)) && not (in_class && c = ch '-') then fail ps "invalid escape";
    if c = ch 'k' && ps.has_names then fail ps "invalid escape";
    c
  end

(* The value of a class of the v flag's grammar: its code points, its strings (each of a length other than one, the
   empty string among them), and whether its syntax may hold a string. *)
type class_value = { cset : Charset.t; strings : int array list; may_contain_strings : bool }

let empty_value = { cset = Charset.make [||]; strings = []; may_contain_strings = false }
let value_of cset = { cset; strings = []; may_contain_strings = false }

let value_union a b =
  let strings = List.fold_left (fun acc s -> if List.mem s acc then acc else acc @ [ s ]) a.strings b.strings in
  {
    cset = Charset.union a.cset b.cset;
    strings;
    may_contain_strings = a.may_contain_strings || b.may_contain_strings;
  }

let value_intersect a b =
  {
    cset = Charset.intersect a.cset b.cset;
    strings = List.filter (fun s -> List.mem s b.strings) a.strings;
    may_contain_strings = a.may_contain_strings && b.may_contain_strings;
  }

let value_subtract a b =
  {
    cset = Charset.subtract a.cset b.cset;
    strings = List.filter (fun s -> not (List.mem s b.strings)) a.strings;
    may_contain_strings = a.may_contain_strings;
  }

(* A string as its code points, each canonicalized in a case-insensitive group so that strings are compared as
   ECMA-262 compares them. *)
let canonical_string ps code_points =
  Array.map (fun c -> if ps.icase then Fold.canonicalize c true else c) code_points

(* The value of a property of strings: its code points, and its sequences (case folded in a case-insensitive group). *)
let strings_value ps cset strings =
  { cset; strings = List.map (canonical_string ps) strings; may_contain_strings = true }

(* The node of a class of the v flag's grammar: its set, or, where it holds strings, the alternatives of its strings
   longest first and then its set, as ECMA-262 orders them, in a group. *)
let class_node ps value negated =
  if negated then set_of (Charset.complement value.cset)
  else if value.strings = [] then set_of value.cset
  else begin
    let ordered = List.stable_sort (fun a b -> compare (Array.length b) (Array.length a)) value.strings in
    let alternatives =
      List.map
        (fun s ->
          charge ps (node_size * (Array.length s + 1));
          if Array.length s = 0 then node Empty else with_subs Cat (Array.map (char_node ps) s))
        ordered
    in
    let alternatives = if Charset.is_empty value.cset then alternatives else alternatives @ [ set_of value.cset ] in
    with_subs Group [| with_subs Alt (Array.of_list alternatives) |]
  end

(* The set operator at the position (& for &&, - for --), or 0. *)
let set_operator ps =
  let n = Array.length ps.p in
  if ps.i + 1 < n && ps.p.(ps.i) = ps.p.(ps.i + 1) && (ps.p.(ps.i) = ch '&' || ps.p.(ps.i) = ch '-') then ps.p.(ps.i)
  else 0

(* Reads one character of a class of the v flag's grammar: a character that is not a syntax character and does not
   begin a reserved double punctuator, or an escape, which may be of a reserved punctuator. *)
let class_set_character ps =
  if not (more ps) then fail ps "unterminated character class";
  let n = Array.length ps.p in
  let c = ps.p.(ps.i) in
  if c = ch '\\' then begin
    if ps.i + 1 >= n then fail ps "\\ at end of pattern";
    let e = ps.p.(ps.i + 1) in
    if e = ch 'b' then begin
      ps.i <- ps.i + 2;
      0x08
    end
    else if in_ascii e "&-!#%,:;<=>@`~" then begin
      ps.i <- ps.i + 2;
      e
    end
    else begin
      ps.i <- ps.i + 1;
      character_escape ps true
    end
  end
  else begin
    if in_ascii c "()[]{}/-\\|" then fail ps "invalid character in character class";
    if ps.i + 1 < n && ps.p.(ps.i + 1) = c && in_ascii c "&!#$%*+,.:;<=>?@^`~" then
      fail ps "invalid set operation in character class";
    ps.i <- ps.i + 1;
    c
  end

(* Reads the strings of \q{...} after its \q. A string of one character is a character of the class. *)
let class_string_disjunction ps =
  if not (eat ps '{') then fail ps "invalid escape";
  let current = Buffer_int.create 8 in
  let value = ref empty_value in
  let fin = ref false in
  while not !fin do
    if not (more ps) then fail ps "unterminated character class";
    let c = peek ps in
    if c = ch '|' || c = ch '}' then begin
      ps.i <- ps.i + 1;
      if Buffer_int.length current = 1 then begin
        let x = Buffer_int.get current 0 in
        let set = fold_if ps (Charset.make [| x; x |]) in
        value := { !value with cset = Charset.union !value.cset set }
      end
      else begin
        charge ps (node_size * max 1 (Buffer_int.length current));
        let s = canonical_string ps (Buffer_int.contents current) in
        let strings = if List.mem s !value.strings then !value.strings else !value.strings @ [ s ] in
        value := { !value with may_contain_strings = true; strings }
      end;
      Buffer_int.clear current;
      if c = ch '}' then fin := true
    end
    else Buffer_int.add current (class_set_character ps)
  done;
  !value

(* Reads one element of a class of the v flag's grammar: an operand, or a range of two characters, which it reports. *)
let rec class_set_element ps =
  charge ps node_size;
  let n = Array.length ps.p in
  let c = peek ps in
  if c = ch '[' then begin
    (* A nested class: its strings count as strings whether or not there are any. *)
    let start = ps.i in
    ps.depth <- ps.depth + 1;
    if ps.depth > max_nesting then raise (Too_large (Nesting, max_nesting));
    ps.i <- ps.i + 1;
    let negated = eat ps '^' in
    let inner = class_set_contents ps in
    if not (eat ps ']') then fail ps "unterminated character class";
    ps.depth <- ps.depth - 1;
    if negated then begin
      if inner.may_contain_strings then begin
        ps.i <- start;
        fail ps "negated character class may contain strings"
      end;
      (value_of (Charset.complement inner.cset), false)
    end
    else (inner, false)
  end
  else begin
    let escaped =
      if c = ch '\\' then begin
        if ps.i + 1 >= n then fail ps "\\ at end of pattern";
        let e = ps.p.(ps.i + 1) in
        if e = ch 'q' then begin
          ps.i <- ps.i + 2;
          Some (class_string_disjunction ps, false)
        end
        else begin
          ps.i <- ps.i + 1;
          match class_escape ps e with
          | Some set -> (
              match ps.escape_strings with
              | Some strings ->
                  let set = fold_if ps set in
                  Some (strings_value ps set strings, false)
              | None ->
                  (* A class escape is folded as one operand; \P is already the complement of a folded set. *)
                  let set = if e = ch 'P' then set else fold_if ps set in
                  Some (value_of set, false))
          | None ->
              ps.i <- ps.i - 1;
              None
        end
      end
      else None
    in
    match escaped with
    | Some v -> v
    | None ->
        let first = class_set_character ps in
        if peek ps = ch '-' && set_operator ps = 0 then begin
          let dash = ps.i in
          ps.i <- ps.i + 1;
          let next = peek ps in
          let escape_class = next = ch '\\' && ps.i + 1 < n && in_ascii ps.p.(ps.i + 1) "qdDwWsSpP" in
          if next = ch ']' || next = ch '[' || escape_class then begin
            ps.i <- dash;
            fail ps "invalid character class range"
          end;
          let second = class_set_character ps in
          if second < first then begin
            ps.i <- dash;
            fail ps "range out of order in character class"
          end;
          (value_of (fold_if ps (Charset.make [| first; second |])), true)
        end
        else (value_of (fold_if ps (Charset.make [| first; first |])), false)
  end

(* Reads the contents of a class of the v flag's grammar, up to its ]. *)
and class_set_contents ps =
  if peek ps = ch ']' then empty_value
  else begin
    let value, range = class_set_element ps in
    let value = ref value in
    let op = set_operator ps in
    if op <> 0 then begin
      (* An intersection or a difference: operands only, with the one operator between every two. *)
      if range then fail ps "invalid set operation in character class";
      while set_operator ps = op do
        ps.i <- ps.i + 2;
        if peek ps = ch ']' || not (more ps) then fail ps "invalid set operation in character class";
        let operand, operand_range = class_set_element ps in
        if operand_range then fail ps "invalid set operation in character class";
        charge ps (range_size * (Array.length !value.cset.ranges + Array.length operand.cset.ranges) / 2);
        value := if op = ch '&' then value_intersect !value operand else value_subtract !value operand
      done;
      if peek ps <> ch ']' then fail ps "invalid set operation in character class";
      !value
    end
    else begin
      (* A union. *)
      while more ps && peek ps <> ch ']' do
        if set_operator ps <> 0 then fail ps "invalid set operation in character class";
        let operand, _ = class_set_element ps in
        charge ps (range_size * (Array.length !value.cset.ranges + Array.length operand.cset.ranges) / 2);
        value := value_union !value operand
      done;
      !value
    end
  end

(* Reads a class of the v flag's grammar from its [ to its ]. *)
let class_set_expression ps =
  ps.i <- ps.i + 1;
  let negated = eat ps '^' in
  let start = ps.i in
  let value = class_set_contents ps in
  if not (eat ps ']') then fail ps "unterminated character class";
  if negated && value.may_contain_strings then begin
    ps.i <- start;
    fail ps "negated character class may contain strings"
  end;
  class_node ps value negated

(* Reads one member of a class. It returns a code point, or a set for a class escape. *)
let class_atom ps =
  let c = peek ps in
  ps.i <- ps.i + 1;
  if c <> ch '\\' then (c, None)
  else begin
    if not (more ps) then fail ps "\\ at end of pattern";
    let e = peek ps in
    if e = ch 'b' then begin
      ps.i <- ps.i + 1;
      (0x08, None)
    end
    else
      match class_escape ps e with Some set -> (0, Some set) | None -> (character_escape ps true, None)
  end

(* Reads a character class from its [ to its ]. *)
let class_ ps =
  if ps.sets then class_set_expression ps
  else begin
    ps.i <- ps.i + 1;
    let negated = eat ps '^' in
    let pairs = Buffer_int.create 16 in
    (* The ranges read so far that the size of the pattern has been charged for. *)
    let charged = ref 0 in
    let fin = ref false in
    while not !fin do
      if not (more ps) then fail ps "unterminated character class";
      if eat ps ']' then fin := true
      else begin
        charge ps (range_size * ((Buffer_int.length pairs / 2) - !charged));
        charged := Buffer_int.length pairs / 2;
        let first, first_set = class_atom ps in
        if peek ps = ch '-' && ps.i + 1 < Array.length ps.p && ps.p.(ps.i + 1) <> ch ']' then begin
          ps.i <- ps.i + 1;
          let second, second_set = class_atom ps in
          if first_set <> None || second_set <> None then begin
            if ps.unicode then fail ps "invalid character class range";
            (* Annex B reads the '-' as itself when a class escape is at either end. *)
            append_class_atom pairs first first_set;
            Buffer_int.add pairs (ch '-');
            Buffer_int.add pairs (ch '-');
            append_class_atom pairs second second_set
          end
          else begin
            if second < first then fail ps "range out of order in character class";
            Buffer_int.add pairs first;
            Buffer_int.add pairs second
          end
        end
        else append_class_atom pairs first first_set
      end
    done;
    (* Inside a case-insensitive group a character matches the class when it is equivalent to a member, and a negated
       class excludes exactly those characters. *)
    charge ps (range_size * ((Buffer_int.length pairs / 2) - !charged));
    let set = Charset.make (Buffer_int.contents pairs) in
    let set = if ps.icase then fold_set ps set else set in
    set_of (if negated then Charset.complement set else set)
  end

let rec disjunction ps =
  ps.depth <- ps.depth + 1;
  if ps.depth > max_nesting then raise (Too_large (Nesting, max_nesting));
  ps.disjunctions <- ps.disjunctions + 1;
  let b = { disjunction = ps.disjunctions; alternative = 0 } in
  let saved = ps.path in
  ps.path <- saved @ [ b ];
  let first = alternative ps in
  let result =
    if peek ps <> ch '|' then first
    else begin
      let subs = ref [ first ] in
      while eat ps '|' do
        charge ps node_size;
        b.alternative <- b.alternative + 1;
        subs := alternative ps :: !subs
      done;
      with_subs Alt (Array.of_list (List.rev !subs))
    end
  in
  ps.path <- saved;
  ps.depth <- ps.depth - 1;
  result

and alternative ps =
  let terms = ref [] in
  while more ps && peek ps <> ch '|' && peek ps <> ch ')' do
    terms := term ps :: !terms
  done;
  match !terms with [] -> node Empty | [ t ] -> t | ts -> with_subs Cat (Array.of_list (List.rev ts))

and term ps =
  (* The atom, and the repetition or the group around it. *)
  charge ps (2 * node_size);
  let c = peek ps in
  let quantifiable = ref true in
  let atom =
    if c = ch '^' then begin
      ps.i <- ps.i + 1;
      quantifiable := false;
      if ps.multiline then begin
        ps.needs_backtracking <- true;
        node Line_start
      end
      else node Bol
    end
    else if c = ch '$' then begin
      ps.i <- ps.i + 1;
      quantifiable := false;
      if ps.multiline then begin
        ps.needs_backtracking <- true;
        node Line_end
      end
      else node Eol
    end
    else if c = ch '(' then begin
      let atom, q = group ps in
      quantifiable := q;
      atom
    end
    else if c = ch '.' then begin
      ps.i <- ps.i + 1;
      set_of (if ps.dot_all then any_set else Charset.dot_set)
    end
    else if c = ch '[' then class_ ps
    else if c = ch '\\' then begin
      let atom, q = atom_escape ps in
      quantifiable := q;
      atom
    end
    else if c = ch '*' || c = ch '+' || c = ch '?' then fail ps "nothing to repeat"
    else if c = ch '{' then begin
      if ps.unicode || looks_like_quantifier ps then fail ps "nothing to repeat";
      ps.i <- ps.i + 1;
      char_node ps (ch '{')
    end
    else if c = ch ']' || c = ch '}' then begin
      if ps.unicode then fail ps "lone bracket";
      ps.i <- ps.i + 1;
      char_node ps c
    end
    else begin
      ps.i <- ps.i + 1;
      char_node ps c
    end
  in
  match quantifier ps with
  | None -> atom
  | Some (min, max, lazy_) ->
      if not !quantifiable then fail ps "nothing to repeat";
      { (with_subs Repeat [| atom |]) with min; max; lazy_ }

(* Reads a group or a lookaround. It also reports whether a quantifier may follow. *)
and group ps =
  ps.i <- ps.i + 1;
  if not (eat ps '?') then (capture ps None, true)
  else if eat ps ':' then begin
    let body = disjunction ps in
    close ps;
    (with_subs Group [| body |], true)
  end
  else begin
    let c = peek ps in
    if c = ch 'i' || c = ch 'm' || c = ch 's' || c = ch '-' then (modifier_group ps, true)
    else if eat ps '=' || eat ps '!' then begin
      let negative = ps.p.(ps.i - 1) = ch '!' in
      let body = disjunction ps in
      close ps;
      ps.needs_backtracking <- true;
      (* Annex B lets a quantifier follow a lookahead. The u flag does not. *)
      ({ (with_subs Look [| body |]) with negative }, not ps.unicode)
    end
    else if eat ps '<' then begin
      if eat ps '=' || eat ps '!' then begin
        let negative = ps.p.(ps.i - 1) = ch '!' in
        let body = disjunction ps in
        close ps;
        ps.needs_backtracking <- true;
        ({ (with_subs Look [| body |]) with negative; behind = true }, false)
      end
      else begin
        let name = group_name ps in
        (* Two groups may share a name only when they are in separate alternatives of one disjunction, so that no
           match can go through both. *)
        (match Hashtbl.find_opt ps.names name with
        | Some others ->
            let here = Array.of_list (List.map (fun b -> { b with alternative = b.alternative }) ps.path) in
            List.iter
              (fun other -> if not (separate_alternatives other.npath here) then fail ps "duplicate group name")
              others
        | None -> ());
        (capture ps (Some name), true)
      end
    end
    else fail ps "invalid group"
  end

(* Reads a group that turns modifiers on or off for its body, such as (?i:...) or (?s-i:...). The position is after the
   (?. *)
and modifier_group ps =
  let icase = ps.icase and multiline = ps.multiline and dot_all = ps.dot_all in
  let seen = Array.make 3 false in
  let removing = ref false and count = ref 0 in
  while not (eat ps ':') do
    let c = peek ps in
    ps.i <- ps.i + 1;
    let flag =
      if c = ch 'i' then begin
        ps.icase <- not !removing;
        0
      end
      else if c = ch 'm' then begin
        ps.multiline <- not !removing;
        1
      end
      else if c = ch 's' then begin
        ps.dot_all <- not !removing;
        2
      end
      else if c = ch '-' then begin
        if !removing then fail ps "invalid group modifier";
        removing := true;
        -1
      end
      else fail ps "invalid group modifier"
    in
    if flag >= 0 then begin
      if seen.(flag) then fail ps "repeated group modifier";
      seen.(flag) <- true;
      incr count
    end
  done;
  if !count = 0 then fail ps "invalid group modifier";
  let body = disjunction ps in
  close ps;
  ps.icase <- icase;
  ps.multiline <- multiline;
  ps.dot_all <- dot_all;
  with_subs Group [| body |]

and capture ps name =
  ps.groups <- ps.groups + 1;
  let group = ps.groups in
  (match name with
  | Some name ->
      (* The path is kept, and every group of the same name is compared with it. *)
      let others = match Hashtbl.find_opt ps.names name with Some l -> l | None -> [] in
      charge ps (range_size * (List.length ps.path + List.length others));
      (* The path is copied, since the alternatives of the enclosing disjunctions move on. *)
      let path = Array.of_list (List.map (fun b -> { b with alternative = b.alternative }) ps.path) in
      if others = [] then ps.name_order <- name :: ps.name_order;
      Hashtbl.replace ps.names name ({ ngroup = group; npath = path } :: others)
  | None -> ());
  let body = disjunction ps in
  close ps;
  { (with_subs Group [| body |]) with group }

(* Reads an escape outside a class. It also reports whether a quantifier may follow. *)
and atom_escape ps =
  ps.i <- ps.i + 1;
  if not (more ps) then fail ps "\\ at end of pattern";
  let c = peek ps in
  if c = ch 'b' || c = ch 'B' then begin
    ps.i <- ps.i + 1;
    let boundary = node (if c = ch 'B' then Not_word_boundary else Word_boundary) in
    (* With the u flag, a case-insensitive word boundary counts two more characters as word characters. *)
    if ps.unicode && ps.icase then begin
      ps.needs_backtracking <- true;
      ({ boundary with fold = true }, false)
    end
    else (boundary, false)
  end
  else if c = ch 'k' then begin
    (* Annex B reads \k as the letter k in a pattern with no named group. *)
    if ps.unicode || ps.has_names then begin
      ps.i <- ps.i + 1;
      if not (eat ps '<') then fail ps "invalid named reference";
      let name = group_name ps in
      ps.named_refs <- true;
      ps.needs_backtracking <- true;
      ({ (node Backref) with name; fold = ps.icase }, true)
    end
    else begin
      ps.i <- ps.i + 1;
      (char_node ps (ch 'k'), true)
    end
  end
  else if c >= ch '1' && c <= ch '9' then begin
    let start = ps.i in
    let n = number ps in
    if n <= ps.group_count then begin
      ps.needs_backtracking <- true;
      ({ (node Backref) with groups = [| n |]; fold = ps.icase }, true)
    end
    else begin
      if ps.unicode then fail ps "reference to a group that does not exist";
      (* Annex B reads it as an octal escape, or as the digit itself. *)
      ps.i <- start;
      let octal = legacy_octal ps in
      (char_node ps octal, true)
    end
  end
  else
    match class_escape ps c with
    | Some set -> (
        match ps.escape_strings with
        | Some strings ->
            (* A property of strings (the v flag): the class of its code points and its sequences. *)
            let set = fold_if ps set in
            let value = strings_value ps set strings in
            (class_node ps value false, true)
        | None -> (set_node ps set, true))
    | None ->
        let escaped = character_escape ps false in
        (char_node ps escaped, true)

(* Reads the pattern, which the caller has checked is UTF-8. It raises Syntax for a pattern that is not valid under
   the chosen grammar, and Too_large for a pattern over a limit of size, which it stops reading at the limit. *)
let parse_with pattern (syntax : syntax) =
  let unicode = syntax.unicode in
  if String.length pattern > max_pattern_length then raise (Too_large (Length, max_pattern_length));
  let p = code_points pattern in
  let p = if unicode then p else code_units p in
  let ps =
    {
      p;
      i = 0;
      unicode;
      sets = syntax.sets;
      escape_strings = None;
      depth = 0;
      group_count = 0;
      has_names = false;
      groups = 0;
      names = Hashtbl.create 8;
      name_order = [];
      named_refs = false;
      path = [];
      disjunctions = 0;
      icase = syntax.icase;
      multiline = syntax.multiline;
      dot_all = syntax.dot_all;
      needs_backtracking = false;
      size = 0;
    }
  in
  scan_groups ps;
  let root = disjunction ps in
  if ps.i < Array.length ps.p then begin
    if ps.p.(ps.i) = ch ')' then fail ps "unmatched ')'";
    fail ps "unexpected character"
  end;
  let root = if ps.named_refs then resolve ps root else root in
  (root, ps)
