(** Operations on types. *)

(** Ground types. *)
module Ground = struct
  type t = Int | String

  let to_string = function
    | Int -> "int"
    | String -> "string"
end

(** Level for type variables. *)
type level = int

(** Variables. *)
type var = {
  id : int; (* a unique identifier *)
  level : level; (* level for generalization *)
  mutable lower : t list; (* lower bound *)
  mutable upper : t list (* upper bound *)
}

(** Types. *)
and t =
  | Var of var
  | Ground of Ground.t
  | Arr of t * t
  | Record of (string * t) list

(** Type scheme: variables strictly above the level should be instantiated. *)
and scheme = level * t

let rec to_string = function
  | Var x ->
    if x.lower = [] && x.upper = [] then "'a" ^ string_of_int x.id
    else
      let lower = List.map to_string x.lower |> String.concat ", " in
      let upper = List.map to_string x.upper |> String.concat ", " in
      Printf.sprintf "'a%d@(%s < %s)" x.id lower upper
  | Ground g -> Ground.to_string g
  | Arr (a, b) -> to_string a ^ " -> " ^ to_string b
  | Record r ->
    let r = List.map (fun (l,a) -> l ^ " : " ^ to_string a) r |> String.concat ", " in
    "{" ^ r ^ "}"

(** Equality between variables. *)
let var_eq (x:var) (y:var) =
  (* we want _physical_ equality here *)
  x == y

(** Equality between types. *)
(* Note: we cannot use standard equality here, because it will recurse
   infinitely when looking at Arr (..., Var x) = Arr (..., Var x): it will look
   when testing whether Var x is the same as Var x. Note that this will not
   happen for Var x at toplevel because it seems that OCaml tests for physical
   equality first. *)
let rec eq t u =
  (* The style of the match is a bit heavy but we want to avoid having an _ at
     the end in order to detect it if we extend the type t. *)
  match t, u with
  | Var x, Var y -> var_eq x y
  | Var _, _ -> false
  | Ground t, Ground u -> t = u
  | Ground _, _ -> false
  | Arr (t, u), Arr (t', u') -> eq t t' && eq u u'
  | Arr _, _ -> false
  | Record r, Record r' -> List.length r = List.length r' && List.for_all2 (fun (l,t) (l',t') -> l = l' && eq t t') r r'
  | Record _, _ -> false

let scheme_of_type a : scheme = max_int, a

let invar =
  let id = ref (-1) in
  fun ?(lower=[]) ?(upper=[]) level ->
    incr id;
    { id = !id; level; lower; upper }

(** Create a fresh variable. *)
let var ?lower ?upper level =
  Var (invar ?lower ?upper level)

(** A typing error. *)
exception Error of string

(** Level of a type. *)
(* TODO: this could be memoized or associated to the type for efficiency *)
let rec level = function
  | Ground _ -> 0
  | Var x -> x.level
  | Arr (a, b) -> max (level a) (level b)
  | Record r -> List.fold_left (fun l (_,a) -> max l (level a)) 0 r

(** Polarity of a variable (true means positive). *)
type polarity = bool

(** A variable with a polarity. *)
type pvar = var * polarity

(** Equality on variables with polarity. *)
let pvar_eq ((x,p):pvar) ((x',p'):pvar) = var_eq x x' && p = p'

(** Copies a type up to its type variables of wrong level (and their extruded
    bounds). *)
let extrude lvl polarity a =
  if level a <= lvl then a else
    let l = ref [] in
    let rec aux p = function
      | Ground _ -> a
      | Arr (a, b) -> Arr (aux (not p) a, aux p b)
      | Record r ->
        let r = List.map (fun (l,a) -> l, aux p a) r in
        Record r
      | Var x ->
        (
          match List.find_map (fun (y,a) -> if pvar_eq (x,p) y then Some a else None) !l with
          | Some a -> a
          | None ->
            let x' = invar lvl in
            if p then
              (
                x.upper <- Var x' :: x.upper;
                x'.lower <- List.map (aux p) x.lower;
              )
            else
              (
                x.lower <- Var x' :: x.lower;
                x'.upper <- List.map (aux p) x.upper;
              );
            Var x'
        )
    in
    aux polarity a

(** Ensure that the first type is a subtype of the second. *)
let rec ( <: ) =
  let cache = ref [] in
  fun a b ->
    (* It is crucial that we cache subtyping here because variables can occur in their own bounds. *)
    if List.exists (fun (a',b') -> eq a a' && eq b b') !cache then () else
      (
        cache := (a,b) :: !cache;
        match a, b with
        | Ground a, Ground b when a = b -> ()
        | Arr (a, b), Arr (a', b') -> a' <: a; b <: b'
        | Record r, Record s ->
          (* the fields of the _second_ should be present in the first *)
          List.iter (fun (l, a) -> try List.assoc l r <: a with Not_found -> raise (Error ("missing field: " ^ l))) s
        | Var x, b ->
          if level b <= x.level then
            (
              x.upper <- b::x.upper;
              List.iter (fun a -> a <: b) x.lower
            )
          else
            a <: extrude x.level false b
        | a, Var x ->
          if level a <= x.level then
            (
              x.lower <- a::x.lower;
              List.iter (fun b -> a <: b) x.upper
            )
          else
            (
              extrude x.level true a <: b
            )
        | _ -> raise (Error (Printf.sprintf "got %s but %s expected" (to_string a) (to_string b)))
      )

(** Instantiate a type scheme as a type. *)
let instantiate level ((l,a):scheme) =
  let fresh = ref [] in
  let rec aux = function
    | Var x ->
      if x.level <= l then Var x else
        (
          match List.find_map (fun (y, y') -> if var_eq x y then Some y' else None) !fresh with
          | Some x' -> x'
          | None ->
            (* Things have to be performed in this order because a variable can
               occur as a lower of itself. *)
            let x' = invar level in
            fresh := (x, Var x') :: !fresh;
            x'.lower <- List.map aux x.lower;
            x'.upper <- List.map aux x.upper;
            Var x'
        )
    | Ground g -> Ground g
    | Arr (a, b) -> Arr (aux a, aux b)
    | Record r -> Record (List.map (fun (l,a) -> l, aux a) r)
  in
  if l = max_int then a else aux a

(** Infer the type of a term. *)
let rec infer ?(level=0) (env:(string*scheme) list) t =
  let infer ?(level=level) = infer ~level in
  match t with
  | Lang.Int _ -> Ground Int
  | Lang.String _ -> Ground String
  | Var x -> (try instantiate level (List.assoc x env) with Not_found -> failwith ("Unbound variable " ^ x))
  | Abs (x, t) ->
    let a = var level in
    let b = infer ((x,(scheme_of_type a))::env) t in
    Arr (a, b)
  | App (t, u) ->
    let a = infer env u in
    let b = var level in
    infer env t <: Arr (a, b);
    b
  | Record r ->
    let r = List.map (fun (l, t) -> l, infer env t) r in
    Record r
  | Field (t, l) ->
    let a = var level in
    infer env t <: Record [l, a];
    a
  | Let (r, x, t, u) ->
    let a =
      if r then
        let a = var (level+1) in
        let env = (x,(scheme_of_type a))::env in
        let a' = infer ~level:(level+1) env t in
        a' <: a;
        a'
      else
        infer ~level:(level+1) env t
    in
    infer ((x,(level,a))::env) u
