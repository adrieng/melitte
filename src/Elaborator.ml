module R = Raw
module S = Syntax

module Env = struct
  type 'a t = { len : int; names : (R.pattern_desc * 'a) list; }

  let empty = { len = 0; names = []; }

  let lookup pos x env =
    let rec loop env =
      match env with
      | [] -> Error.unbound_identifier pos x
      | (R.PWildcard, _) :: env -> loop env
      | (R.PVar y, v) :: env -> if x = y then v else loop env
    in
    loop env.names

  let extend p v env =
    { len = env.len + 1; names = (p, v) :: env.names; }

  let map f env =
    { env with names = List.map (function (p, v) -> (p, f v)) env.names; }
end

type value =
  | Reflect of { ty : value; tm : neutral; }
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
  | Reify of { ty : value; tm : value; }

and clo = C of R.pattern * R.term * value Env.t

and env = value Env.t

module M = struct
  type 'a t = env -> 'a * env

  let return x = fun env -> x, env

  let bind (type a b) (x : a t) (f : a -> b t) : b t =
    fun env -> let y, env = x env in f y env

  let run x = let y, _ = x Env.empty in y
end

module Infix = Monad.Notation (M)

include M

(* {2 Evaluation} *)

let close env (p, t) = C (p, t, env)

let rec eval : R.term -> env -> value =
  fun t env ->
  match t.Position.value with
  | R.Var x ->
     Env.lookup t.Position.position x env

  | R.Lam t ->
     Lam (close env t)

  | R.App (t, u) ->
     eval_app (eval t env) (eval u env)

  | R.Forall (a, b) ->
     Forall (eval a env, close env b)

  | R.Let { bound; body; _ } ->
     eval_clo (close env body) (eval bound env)

  | R.Type ->
     Type

  | R.Nat ->
     Nat

  | R.Zero ->
     Zero

  | R.Succ t ->
     Succ (eval t env)

  | R.Natelim { discr; motive; case_zero; case_succ; } ->
     eval_nat_elim
       (eval discr env)
       (close env motive)
       (eval case_zero env)
       (close env case_succ)

and eval_app v w =
  match v with
  | Lam c ->
     eval_clo c w
  | Reflect { ty = Forall (a, b); tm; } ->
     Reflect { ty = eval_clo b w; tm = App (tm, Reify { ty = a; tm = w; }); }
  | _ ->
     assert false               (* type error *)

and eval_nat_elim d m u0 uN =
  match d with
  | Zero ->
     u0
  | Succ n ->
     eval_clo uN n
  | Reflect { ty = Nat; tm; } ->
     Reflect { ty = Nat; tm = Natelim (tm, m, u0, uN); }
  | _ ->
     assert false               (* type error *)

and eval_clo (C (p, t, env)) v = eval t (Env.extend p.value v env)

(* {2 Quotation} *)

let fresh ty n = Reflect { ty; tm = Var n; }, n + 1

let rec quote_ne : neutral -> int -> S.term =
  fun v n ->
  let t_desc =
    match v with
    | Var k ->
       S.Var (n - (k + 1))

    | App (ne, nf) ->
       S.App (quote_ne ne n, quote_nf nf n)

    | Natelim (discr, motive, case_zero, case_succ) ->
       let discr = quote_ne discr n in
       let case_zero = quote_nf (Reify { ty = eval_clo motive Zero;
                                         tm = case_zero; }) n in
       let case_succ = quote_clo_nf ~bound_ty:Nat ~ty:motive case_succ n in
       let motive = quote_clo_ty ~bound_ty:Nat motive n in
       S.Natelim { discr; motive; case_zero; case_succ; }
  in
  S.{ t_desc; t_loc = Position.dummy; }

and quote_nf : normal -> int -> S.term =
  fun (Reify { ty; tm; }) n ->
  match ty, tm with
  | _, Reflect { tm; _ } ->
     quote_ne tm n

  | Type, _ ->
     quote_ty tm n

  | Forall (a, f), v ->
     let x, n = fresh a n in
     S.Build.lam
       (Bound1 { body = quote_nf (Reify { ty = eval_clo f x;
                                          tm = eval_app v x; }) n;
                 user = None; })

  | Nat, Zero ->
     S.Build.zero

  | Nat, Succ tm ->
     S.Build.succ (quote_nf (Reify { ty; tm; }) n)

  | _, Lam _ ->
     assert false             (* eliminated by η-expansion  *)

  | _ ->
     assert false             (* ill-typed *)

and quote_ty : value -> int -> S.term =
  fun v n ->
  let t_desc =
    match v with
    | Type ->
       S.Type

    | Nat ->
       S.Nat

    | Forall (a, f) ->
       S.Forall (quote_ty a n, quote_clo_ty f ~bound_ty:a n)

    | Reflect _ | Zero | Succ _ | Lam _ ->
       invalid_arg "quote_ty"
  in
  { t_desc; t_loc = Position.dummy; }

and quote_clo_ty ~bound_ty clo n =
  let x, n = fresh bound_ty n in
  S.Bound1 { body = quote_ty (eval_clo clo x) n; user = None; }

and quote_clo_nf ~bound_ty ~ty clo env =
  let x, n = fresh bound_ty env in
  S.Bound1 { body = quote_nf (Reify { ty = eval_clo ty x;
                                      tm = eval_clo clo x; }) n;
                  user = None; }

(* {2 Normalization} *)

let reflect_cx : R.ty Env.t -> value Env.t =
  fun Env.{ len; names; } ->
  let rec loop len names =
    match names with
    | [] -> Env.empty
    | (p, a) :: names ->
       let env = loop (len - 1) names in
       Env.extend p (Reflect { ty = eval a env; tm = Var len; }) env
  in
  loop len names

let nf ~env ~ty t =
  let env = reflect_cx env in
  quote_nf (Reify { ty = eval ty env; tm = eval t env; }) env.len

(* {2 Type checking} *)

let check : R.t -> S.t t =
  fun _r -> return []
