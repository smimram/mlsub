(** User-friendly representation of types. *)

(** Ground types. *)
type ground = Type.ground

(** A type representation. *)
type t =
  | Top | Bot
  | Union of t * t
  | Inter of t * t
  | Ground of ground
  | Fun of t * t
  | Record of (string * t) list
  | Recursive of string * t
  | Var of string

(** Polarity of a variable (true means positive). *)
type polarity = bool

(** Compute the representation of a type. *)
(* let make t = *)
  
