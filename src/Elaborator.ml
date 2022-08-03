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
  | Natelim of neutral * clo * value * clo

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

(* {2 Evaluation} *)

let close env (p, t) = C (p, t, env)

let rec eval : Raw.term -> env -> value =
  fun t env ->
  match t.Position.value with
  | Raw.Var x ->
     Env.lookup t.Position.position x env

  | Raw.Lam t ->
     Lam (close env t)

  | Raw.App (t, u) ->
     eval_app (eval t env) (eval u env)

  | Raw.Forall (a, b) ->
     Forall (eval a env, close env b)

  | Raw.Let { bound; body; _ } ->
     eval_clo (close env body) (eval bound env)

  | Raw.Type ->
     Type

  | Raw.Nat ->
     Nat

  | Raw.Zero ->
     Zero

  | Raw.Succ t ->
     Succ (eval t env)

  | Raw.Natelim { discr; motive; case_zero; case_succ; } ->
     eval_nat_elim
       (eval discr env)
       (close env motive)
       (eval case_zero env)
       (close env case_succ)

and eval_app v w =
  match v with
  | Lam c ->
     eval_clo c w
  | Reflect { typ = Forall (a, b); tm; } ->
     Reflect { typ = eval_clo b w; tm = App (tm, Reify { typ = a; tm = w; }); }
  | _ ->
     assert false               (* type error *)

and eval_nat_elim d m u0 uN =
  match d with
  | Zero ->
     u0
  | Succ n ->
     eval_clo uN n
  | Reflect { typ = Nat; tm; } ->
     Reflect { typ = Nat; tm = Natelim (tm, m, u0, uN); }
  | _ ->
     assert false               (* type error *)

and eval_clo (C (p, t, env)) v = eval t (Env.extend p v env)

(* {2 Quotation} *)

type quote_env = unit Env.t

let quote_bind a env =
  Reflect { typ = a; tm = Var env.Env.len; },
  Env.extend Position.(unknown_pos Raw.PWildcard) () env

let rec quote_ne : neutral -> quote_env -> Syntax.term =
  fun v env ->
  let t_desc =
    match v with
    | Var k ->
       Syntax.Var (env.Env.len - (k + 1))

    | App (ne, nf) ->
       Syntax.App (quote_ne ne env, quote_nf nf env)

    | Natelim (discr, motive, case_zero, case_succ) ->
       let discr = quote_ne discr env in
       let case_zero = quote_nf (Reify { typ = eval_clo motive Zero;
                                         tm = case_zero; }) env in
       let case_succ =
         quote_bound (fun x -> quote_nf (Reify { typ = eval_clo motive x;
                                                 tm = eval_clo case_succ x; }))
           Nat env
       in
       let motive =
         quote_bound (fun x -> quote_ty (eval_clo motive x)) Nat env
       in
       Syntax.Natelim { discr; motive; case_zero; case_succ; }
  in
  Syntax.{ t_desc; t_loc = Position.dummy; }

and quote_nf : normal -> unit Env.t -> Syntax.term =
  fun (Reify { typ; tm; }) env ->
  let mk t_desc = Syntax.{ t_desc; t_loc = Position.dummy; } in
  match typ, tm with
  | _, Reflect { tm; _ } ->
     quote_ne tm env

  | Type, _ ->
     quote_ty tm env

  | Forall (a, f), _ ->
     let quote_body x =
       quote_nf (Reify { typ = eval_clo f x; tm = eval_app tm x; })
     in
     mk (Syntax.Lam (quote_bound quote_body a env))

  | Nat, Zero ->
     Syntax.Build.zero

  | Nat, Succ tm ->
     Syntax.Build.succ (quote_nf (Reify { typ; tm; }) env)

  | _, Lam _ ->
     assert false             (* eliminated by η-expansion  *)

  | _ ->
     assert false             (* ill-typed *)

and quote_ty : value -> unit Env.t -> Syntax.term =
  fun v env ->
  let t_desc =
    match v with
    | Type ->
       Syntax.Type

    | Nat ->
       Syntax.Nat

    | Forall (a, f) ->
       Syntax.Forall (quote_ty a env,
                      quote_bound (fun x -> quote_ty (eval_clo f x)) a env)

    | Reflect _ | Zero | Succ _ | Lam _ ->
       invalid_arg "quote_ty"
  in
  { t_desc; t_loc = Position.dummy; }

and quote_bound f a env =
  let x, env = quote_bind a env in
  Syntax.Bound1 { body = f x env; user = None; }

let check : Raw.t -> Syntax.t t =
  fun _r -> return []
