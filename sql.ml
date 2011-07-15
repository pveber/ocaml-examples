(**
   Some quick thoughts on a typed DSL for SQL queries using GADTs
*)

type _ expr = 
  | IntLit : int -> int expr
  | Equal : 'a expr -> 'a expr -> bool expr
  | And : bool expr -> bool expr -> bool expr
  | Proj : 'a row -> string -> 'b expr (* seems that it cannot be typed in a safer way *)
and 'a row
and 'a view 

val from2 : 'a view -> 'b view -> ('a row -> 'b row -> bool expr) -> ('a row -> 'b row -> 'c row)
