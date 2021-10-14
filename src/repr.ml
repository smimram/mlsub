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

(** Given a strictly positive integer, generate a name in [a-z]+: a, b, ... z,
    aa, ab, ... az, ba, ... *)
let string_of_univ =
  let base = 26 in
  let c i = char_of_int (int_of_char 'a' + i - 1) in
  let add i suffix = Printf.sprintf "%c%s" (c i) suffix in
  let rec n suffix i =
    if i <= base then
      add i suffix
    else
      let head = i mod base in
      let head = if head = 0 then base else head in
      n (add head suffix) ((i-head)/base)
  in
  n ""

(** Compute the representation of a type. *)
let make t =
  (* Generate a name for a variable. *)
  let name =
    let l = ref [] in
    let n = ref 0 in
    fun x ->
      match List.find_map (fun (y,s) -> if Type.var_eq x y then Some s else None) !l with
      | Some s -> s
      | None ->
        incr n;
        let s = "`" ^ string_of_univ !n in
        l := (x,s) :: !l;
        s
  in
  let rec aux polarity processing = function
    | Type.Ground a -> Ground a
    | Arr (a, b) -> Arr (aux (not polarity) processing a, aux polarity processing b)
    | Record r -> Record (List.map (fun (l,a) -> l, aux polarity processing a) r)
    | Var x ->
      if List.exists (Type.pvar_eq (x,polarity)) processing then
        (* This is an occurrence of the variable in one of its own bounds. *)
        failwith "TODO: handle recursive types"
      else
        let bounds = if polarity then x.lower else x.upper in
        let bounds = List.map (aux polarity ((x,polarity)::processing)) bounds in
        let bounds = List.fold_left (fun a b -> if polarity then Union (a, b) else Inter (a, b)) (Var (name x)) bounds in
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
