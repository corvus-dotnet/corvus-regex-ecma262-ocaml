(* A set of code points as sorted, inclusive ranges that neither overlap nor touch: r16 the bounds of the ranges that
   end at or below U+FFFF, and r32 those of the ranges that start above it, two numbers a range. The table type of
   every version's generated module (ucd_tables<major>.ml) is this one. *)
type t = { r16 : int array; r32 : int array }
