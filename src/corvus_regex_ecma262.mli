(** Regular expressions with the grammar and the answers of ECMA-262, the [RegExp] of JavaScript.

    Every flag is supported: [d], [g], [i], [m], [s], [u], [v] and [y], with the grammar of each. Without [u] or [v] a
    pattern is read by the Annex B grammar and matched by UTF-16 code unit, as ECMA-262 matches it; with them, by code
    point. The grammar is that of ECMAScript 2025: lookbehind, named groups (a name may be shared between separate
    alternatives), modifier groups such as [(?i:...)], every Unicode property ECMA-262 lists (from the Unicode tables
    the library carries, {!unicode_version}), and the [v] flag's set notation, strings in a class and properties of
    strings.

    {1 Text}

    A text is UTF-8 in a [string], or UTF-16 in a [string] of UTF-16LE code units (two bytes each, as
    [String.get_utf_16le_uchar] reads them), which may hold lone surrogates. The function's name chooses the encoding
    (the [_utf16] forms take UTF-16), and nothing is transcoded. Positions are 0-based offsets in the units of the
    text: bytes of UTF-8, or UTF-16 code units. Without the [u] or [v] flag a match over UTF-8 can start or end between
    the halves of a character beyond the Basic Multilingual Plane; that position is the offset of the character's third
    byte.

    Text that is not UTF-8 is refused with [Invalid_argument], never read as U+FFFD. A text matched many times can be
    checked once, as a {!utf8_text}, which may also be a range of a string, with no copy.

    {1 Limits}

    A pattern without a backreference is matched in time proportional to its size times the text. A pattern can be
    given a match timeout, as a [Regex] of .NET can, which bounds the time and the backtracking memory of every match:
    a match that reaches it raises {!Match_timeout} or {!Match_memory}. With a timeout and the limits of size on a
    pattern, neither a pattern nor a text from outside can make a program hang. Without one a match has no limit, so
    give one to a pattern or a text that comes from outside, with [?match_timeout] or {!set_default_match_timeout}.

    {1 Threads and domains}

    A compiled pattern may be used by any number of threads and domains at once. A match needs working memory, which it
    takes from its domain on OCaml 5 (each domain keeps one), or from the pattern when no other match holds it; a match
    that finds both held makes its own. {!exec_into} takes the working memory its {!captures} keep. *)

(** {1 Compiling} *)

(** The version of the library. *)
val version : string

(** The version of Unicode the property tables are from. *)
val unicode_version : string

(** The flags of a pattern, as [new RegExp(pattern, flags)] takes them. *)
type flags = {
  has_indices : bool;  (** [d]: accepted for completeness; the groups' positions are always kept. *)
  global : bool;  (** [g]: {!replace} replaces every match. *)
  ignore_case : bool;  (** [i]: case is ignored, by the Canonicalize of the grammar in force. *)
  multiline : bool;  (** [m]: [^] and [$] hold at line terminators. *)
  dot_all : bool;  (** [s]: the dot matches line terminators. *)
  unicode : bool;  (** [u]: the grammar of the [u] flag, and matching by code point. *)
  unicode_sets : bool;  (** [v]: the grammar of the [v] flag, and matching by code point. *)
  sticky : bool;  (** [y]: a match only at the position the search starts from. *)
}

(** Which limit of size a pattern is over. *)
type limit =
  | Length  (** {!max_pattern_length} *)
  | Nesting  (** {!max_nesting} *)
  | Size  (** {!max_size} *)

(** Why a pattern does not compile. *)
type error =
  | Invalid of string  (** The pattern is not valid in the grammar of its flags (the [SyntaxError] of [new RegExp]). *)
  | Flags of string  (** The flags are not valid: a letter that is not a flag, a flag given twice, or [u] with [v]. *)
  | Too_large of { limit : limit; value : int }
      (** The pattern is over a limit of size. It was not read to its end, so nothing is known of whether it is valid. *)
  | Invalid_utf8 of int  (** The pattern is not UTF-8: the offset of its first byte that is not. *)

(** What the error says. *)
val error_message : error -> string

(** The name of a limit: ["length"], ["nesting"] or ["size"]. *)
val limit_name : limit -> string

(** Raised by {!compile_exn}. *)
exception Compile_error of error

(** Raised by a match that has run for longer than the pattern's timeout. *)
exception Match_timeout

(** Raised by a match with a timeout whose backtracking stack has reached {!max_stack} entries. *)
exception Match_memory

(** The longest pattern that is read, in bytes: 1,048,576. *)
val max_pattern_length : int

(** The deepest groups may nest: 1,000. *)
val max_nesting : int

(** The largest size a pattern may have, as the parser counts it: 1,048,576. The syntax tree, the program and the time
    to make them are proportional to it. *)
val max_size : int

(** The most entries the backtracking stack of a match with a timeout may hold: 1,048,576. *)
val max_stack : int

(** A compiled pattern. It is immutable, and safe to share between threads and domains. *)
type t

(** [compile ?flags ?match_timeout pattern] compiles a pattern as [new RegExp(pattern, flags)] compiles it. [flags] is
    a string of the flag letters, such as [""] (the default), ["u"] or ["gimsuy"]. [match_timeout] is the time in
    seconds each match may take before it is abandoned with {!Match_timeout}, which brings a cap on the memory of its
    backtracking with it; [0.] is no limit, and without it the pattern takes {!default_match_timeout}. The clock is read
    every 4,096 steps of the matcher, so a match can go past the timeout by some tens of microseconds. *)
val compile : ?flags:string -> ?match_timeout:float -> string -> (t, error) result

(** {!compile}, raising {!Compile_error}. *)
val compile_exn : ?flags:string -> ?match_timeout:float -> string -> t

(** Whether the pattern is valid with the flags, without compiling it. *)
val is_valid : ?flags:string -> string -> bool

(** Reads flags as [new RegExp] reads them. *)
val parse_flags : string -> (flags, error) result

(** The flags as [RegExp.prototype.flags] writes them, in the order [dgimsuvy]. *)
val flags_to_string : flags -> string

(** Sets the match timeout, in seconds, of every pattern compiled from now on without one of its own; [None] or [0.] is
    no limit, which is the default. A pattern already compiled keeps the timeout it was compiled with. *)
val set_default_match_timeout : float option -> unit

(** The match timeout a pattern compiled now without one of its own takes, in seconds. *)
val default_match_timeout : unit -> float option

(** The source of the pattern. *)
val pattern : t -> string

(** The flags of the pattern. *)
val flags : t -> flags

(** The flags of the pattern as [RegExp.prototype.flags] writes them. *)
val flags_string : t -> string

(** The pattern's match timeout in seconds, or [None]. *)
val match_timeout : t -> float option

(** The number of capturing groups. *)
val group_count : t -> int

(** The names of the groups, in the order of the first group of each name. *)
val group_names : t -> string list

(** Each name with the groups that have it (more than one only in separate alternatives). *)
val group_numbers : t -> (string * int list) list

(** {1 Text} *)

(** UTF-8 text that has been checked: a string, or a range of one, held with no copy. The string must not change while
    the text is in use. *)
type utf8_text

(** [utf8_text ?off ?len s] is the range of [s] from [off] (0 by default) of [len] bytes (to the end by default), or the
    offset from [off] of its first byte that is not UTF-8. Positions in it are from [off]. *)
val utf8_text : ?off:int -> ?len:int -> string -> (utf8_text, int) result

(** {!utf8_text}, raising [Invalid_argument] for text that is not UTF-8. *)
val utf8_text_exn : ?off:int -> ?len:int -> string -> utf8_text

(** {1 Matching}

    Each function searches from [pos] (0 by default; with the [y] flag a match must start there), over UTF-8 for the
    plain form, a {!utf8_text} for the [_text] form, and UTF-16LE for the [_utf16] form. A position inside a character
    means that character with the [u] or [v] flag, as [RegExpBuiltinExec] reads it.

    {!is_match}, {!count} and {!exec_into} allocate nothing once the working memory has grown, but for the two words of
    the option that carries a [pos] given to {!is_match}. A string is checked as UTF-8 and made a text the first time it
    is matched; matched again it is not checked again. *)

(** Whether the pattern matches anywhere from [pos]. *)
val is_match : ?pos:int -> t -> string -> bool

val is_match_text : ?pos:int -> t -> utf8_text -> bool
val is_match_utf16 : ?pos:int -> t -> string -> bool

(** A match: where it and each of its groups start and end, and what each holds. *)
module Match : sig
  type t

  (** Where the match starts and ends. *)
  val start : t -> int

  val end_ : t -> int

  (** The number of capturing groups. *)
  val group_count : t -> int

  (** The text of the match, in the encoding of the text. Over UTF-8, half a character at either end (which only a
      pattern without the [u] or [v] flag can give) is written as U+FFFD. *)
  val value : t -> string

  (** The text of group [i] (0 is the whole match), or [None] when it did not take part. *)
  val group : t -> int -> string option

  (** Where group [i] starts and ends, or [None] when it did not take part. *)
  val span : t -> int -> (int * int) option

  (** The text of the group of the name that took part, or [None]. *)
  val named : t -> string -> string option
end

(** The first match from [pos], or [None]. *)
val find : ?pos:int -> t -> string -> Match.t option

val find_text : ?pos:int -> t -> utf8_text -> Match.t option
val find_utf16 : ?pos:int -> t -> string -> Match.t option

(** The matches of a global search, which moves on past an empty match by a character (a code point with the [u] or
    [v] flag, a code unit without). *)
val matches : t -> string -> Match.t Seq.t

val matches_utf16 : t -> string -> Match.t Seq.t

(** The number of matches of a global search. *)
val count : t -> string -> int

val count_text : t -> utf8_text -> int
val count_utf16 : t -> string -> int

(** Where each group of a match is, written by {!exec_into} without allocating. *)
type captures

(** Captures for the pattern's groups. *)
val captures : t -> captures

(** Matches into the captures: whether there is a match from [pos]. The captures keep the working memory of the matches
    into them, so captures are for one match at a time (one for each thread or domain that matches). [pos] is not
    optional, so that a global search made of these calls makes no option for it. *)
val exec_into : t -> captures -> pos:int -> string -> bool

val exec_into_text : t -> captures -> pos:int -> utf8_text -> bool
val exec_into_utf16 : t -> captures -> pos:int -> string -> bool

(** Where group [i] of the last match into the captures starts and ends (0 is the whole match), or [-1] for a group
    that did not take part. *)
val capture_start : captures -> int -> int

val capture_end : captures -> int -> int

(** {1 Replacing and splitting} *)

(** [String.prototype.replace]: the text with the first match replaced, or every match with the [g] flag. In the
    replacement, [$&] is the match, [$`] and [$'] the text before and after it, [$n] and [$nn] a group, [$<name>] a
    named group, and [$$] a dollar. *)
val replace : t -> string -> string -> string

(** [String.prototype.replaceAll]: every match replaced, whatever the flags. *)
val replace_all : t -> string -> string -> string

val replace_utf16 : t -> string -> string -> string
val replace_all_utf16 : t -> string -> string -> string

(** [RegExp.prototype\[Symbol.split\]]: the pieces between the matches, with what each group of a match captured after
    the piece before it ([None] for a group that did not take part, which JavaScript gives as [undefined]), and at
    most [limit] items (all by default). *)
val split : ?limit:int -> t -> string -> string option list

val split_utf16 : ?limit:int -> t -> string -> string option list
