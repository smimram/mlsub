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
  id : int; (* a unique identifier (only used for printing) *)
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

(** Type scheme: variables above the level should be instantiated. *)
and scheme = level * t

let rec to_string = function
  | Var x -> "'a" ^ string_of_int x.id
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

(** Create a fresh variable. *)
let var =
  let id = ref (-1) in
  fun level ->
    incr id;
    Var { id = !id; level; lower = []; upper = [] }

(** A typing error. *)
exception Error of string

(** Ensure that the first type is a subtype of the second. *)
let rec ( <: ) =
  let cache = ref [] in
  fun a b ->
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
          x.upper <- b::x.upper;
          List.iter (fun a -> a <: b) x.lower
        | a, Var x ->
          x.lower <- a::x.lower;
          List.iter (fun b -> a <: b) x.upper
        | _ -> raise (Error (Printf.sprintf "got %s but %s expected" (to_string a) (to_string b)))
      )

(** Infer the type of a term. *)
let rec infer ?(level=0) env t =
  let infer ?(level=level) = infer ~level in
  match t with
  | Lang.Int _ -> Ground Int
  | Lang.String _ -> Ground String
  | Var x -> (try List.assoc x env with Not_found -> failwith ("Unbound variable " ^ x))
  | Abs (x, t) ->
    let a = var level in
    let b = infer ((x,a)::env) t in
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
    ignore x; ignore t; ignore u;
    if r then failwith "TODO";
    failwith "TODO"
