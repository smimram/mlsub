(** User-friendly representation of types. *)

(** Ground types. *)
module Ground = Type.Ground

(** A variable. *)
type var = string

(** A type representation. *)
type t =
  | Top | Bot
  | Union of t * t
  | Inter of t * t
  | Ground of Ground.t
  | Arr of t * t
  | Record of (string * t) list
  | Recursive of var * t
  | Var of var

(** Polarity of a variable (true means positive). *)
type polarity = bool

(** A variable with a polarity. *)
type pvar = Type.var * polarity

(** Equality on variables with polarity. *)
let pvar_eq ((x,p):pvar) ((x',p'):pvar) = Type.var_eq x x' && p = p'

module PVarMap = Map.Make (struct type t = pvar let compare x y = if pvar_eq x y then 0 else compare x y end)

(** Compute the representation of a type. *)
let make t =
  let rec aux polarity processing = function
    | Type.Ground a -> Ground a
    | Arr (a, b) -> Arr (aux (not polarity) processing a, aux polarity processing b)
    | Record r -> Record (List.map (fun (l,a) -> l, aux polarity processing a) r)
    | Var x ->
      (* TODO: we could generate a better name *)
      let name = "a" ^ string_of_int x.id ^ (if polarity then "+" else "-") in
      if List.exists (pvar_eq (x,polarity)) processing then Var name else
        let bounds = if polarity then x.lower else x.upper in
        let bounds = List.map (aux polarity ((x,polarity)::processing)) bounds in
        let bounds = List.fold_left (fun a b -> if polarity then Union (a, b) else Inter (a, b)) (Var name) bounds in
        (* TODO: handle recursive types *)
        bounds
  in
  aux true [] t

(** String representation. *)
let rec to_string = function
  | Top -> "⊤"
  | Bot -> "⊥"
  | Union (a, b) -> "(" ^ to_string a ^ " ∪ " ^ to_string b ^ ")"
  | Inter (a, b) -> "(" ^ to_string a ^ " ∩ " ^ to_string b ^ ")"
  | Ground g -> Ground.to_string g
  | Arr (a, b) -> "(" ^ to_string a ^ " → " ^ to_string b ^ ")"
  | Record r ->
    let r = List.map (fun (l,a) -> l ^ " : " ^ to_string a) r |> String.concat ", " in
    "{" ^ r ^ "}"
  | Recursive (x, a) -> "(µ" ^ x ^ "." ^ to_string a ^ ")"
  | Var x -> x
