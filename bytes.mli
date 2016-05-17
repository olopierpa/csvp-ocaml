type t;;

val create : int -> t;;

val sub_string : t -> int -> int -> string;;

val set : t -> int -> char -> unit;;

val extend : t -> int -> int -> t;;
