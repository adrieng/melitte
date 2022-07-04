module S = Syntax

module Env = struct
  type 'a t = { len : int;
                names : [`Name of string * 'a | `Wildcard] list; }

  let initial = { len = 0; names = []; }

  let lookup pos x env =
    let rec loop env =
      match env with
      | [] -> Error.unbound_identifier pos x
      | `Wildcard :: env -> loop env
      | `Name (y, v) :: env -> if x = y then v else loop env
    in
    loop env.names

  let extend p v env =
    {
      len = env.len + 1;
      names = (match p.Position.value with
               | Raw.PWildcard -> `Wildcard
               | Raw.PVar x -> `Name (x, v)) :: env.names;
    }
end

type value =
  | Reflect of { typ : value; tm : neutral; }
  | Lam of clo
  | Forall of value * clo
  | Type
  | Nat
  | Zero
  | Succ of value

and neutral =
  | Var of int
  | App of neutral * normal
  | Natrec of neutral * clo * value * clo

and normal =
  | Reify of { typ : value; tm : value; }

and clo = C of Raw.pattern * Raw.term * value Env.t

and env = value Env.t

module M = struct
  type 'a t = env -> 'a * env

  let return x = fun env -> x, env

  let bind (type a b) (x : a t) (f : a -> b t) : b t =
    fun env -> let y, env = x env in f y env

  let run x = let y, _ = x Env.initial in y
end

module Infix = Monad.Notation (M)

include M

let close (p, t) env = C (p, t, env)

let rec eval : Raw.term -> env -> value =
  fun t env ->
  match t.Position.value with
  | Raw.Var x ->
     Env.lookup t.Position.position x env

  | Raw.Lam t ->
     Lam (close t env)

  | Raw.App (t, u) ->
     eval_app (eval t env) (eval u env)

  | Raw.Forall (a, b) ->
     Forall (eval a env, close b env)

  | Raw.Let _ ->
     assert false               (* TODO *)

  | Raw.Type ->
     Type

  | Raw.Nat ->
     Nat

  | Raw.Zero ->
     Zero

  | Raw.Succ ->
     assert false               (* TODO *)

  | Raw.Natelim _ ->
     assert false

and eval_app v w =
  match v with
  | Lam c ->
     eval_clo c w
  | Reflect { typ = Forall (a, b); tm; } ->
     Reflect { typ = eval_clo b w; tm = App (tm, Reify { typ = a; tm = w; }); }
  | _ ->
     assert false

and eval_clo (C (p, t, env)) v = eval t (Env.extend p v env)

let check : Raw.t -> Syntax.t t =
  fun _r -> return []
