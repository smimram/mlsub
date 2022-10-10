(** Programs. *)

(** A variable. *)
type var = string

(** Terms of the language. *)
type t =
  | Bool of bool
  | Int of int
  | String of string
  | Var of var
  | Abs of var * t
  | App of t * t
  | Record of (string * t) list
  | Field of t * string (** field of a record *)
  | Let of bool * string * t * t (* let (recursive?) x = t in u *)

(** A declaration: recursive, variable name, value. *)
type decl = bool * string * t

let rec abs l t =
  match l with
  | x::l -> Abs (x, abs l t)
  | [] -> t

(** String representation of a program. *)
let rec to_string = function
  | Bool b -> string_of_bool b
  | Int n -> string_of_int n
  | String s -> "\"" ^ s ^ "\""
  | Var x -> x
  | Abs (x, t) -> Printf.sprintf "(fun %s -> %s)" x (to_string t)
  | App (t, u) -> Printf.sprintf "(%s %s)" (to_string t) (to_string u)
  | Record r -> Printf.sprintf "{%s}" (List.map (fun (l, t) -> Printf.sprintf "%s = %s" l (to_string t)) r |> String.concat ", ")
  | Field (t, l) -> Printf.sprintf "(%s.%s)" (to_string t) l
  | Let (r, x, t, u) -> Printf.sprintf "let%s %s = %s in %s" (if r then " rec" else "") x (to_string t) (to_string u)
