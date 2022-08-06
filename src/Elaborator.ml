module R = Raw
module S = Syntax

module Env = DeBruijn.Env

type value =
  | Reflect of { ty : value; tm : neutral; }
  | Lam of clo1
  | Forall of value * clo1
  | Type
  | Nat
  | Zero
  | Succ of value

and neutral =
  | Var of DeBruijn.Lv.t
  | App of neutral * normal
  | Natelim of neutral * clo1 * value * clo2

and normal =
  | Reify of { ty : value; tm : value; }

and clo1 = C1 of env * S.bound1

and clo2 = C2 of env * S.bound2

(** Names are not used for evaluation since we only evaluate core syntax. Hence,
    environments are accessed via De Bruijn indices. *)
and env = entry Env.t

and entry = {
    v : value;                  (** Used for evaluation. *)
    ty : ty option;             (** Used only during type checking, in which
                                    case this field is never [None]. *)
    n : Name.t;                 (** Used only for scoping. *)
  }

and ty = value

(* {2 Utilities} *)

let clo1_name (C1 (_, S.Bound1 { user; _ })) =
  Option.value ~default:Name.dummy user

let fresh ty free =
  Reflect { ty; tm = Var DeBruijn.Lv.(fresh ~free); }

let find loc x env =
  try Env.find (fun en -> en.n = x) env
  with Not_found -> Error.unbound_identifier loc x

let extend ?ty ?user v env =
  Env.extend { v; ty; n = Option.value ~default:Name.dummy user; } env

let extend_pat ?ty pat v env =
  Env.extend { v; ty; n = R.name_of_pattern pat; } env

let extend_typed ?v ?user ty env =
  let v = match v with
    | None ->
       Reflect { ty; tm = Var DeBruijn.Lv.(fresh ~free:(Env.width env)); }
    | Some v ->
       v
  in
  extend ~ty ?user v env

let extend_typed_pat ?v pat ty env =
  extend_typed ?v ~user:(R.name_of_pattern pat) ty env

(* {2 Evaluation} *)

let close1 b1 env = C1 (env, b1)

let close2 b2 env = C2 (env, b2)

let rec eval : S.term -> env -> value =
  fun t env ->
  match t.t_desc with
  | S.Var ix ->
     begin try (Env.lookup env ix).v
           with Not_found -> Error.internal "ill-scoped evaluation" end

  | S.Lam tm ->
     Lam (close1 tm env)

  | S.App (t, u) ->
     eval_app (eval t env) (eval u env)

  | S.Forall (a, b) ->
     Forall (eval a env, close1 b env)

  | S.Let { def; body; _ } ->
     eval_clo1 (close1 body env) (eval def env)

  | S.Type ->
     Type

  | S.Nat ->
     Nat

  | S.Zero ->
     Zero

  | S.Succ t ->
     Succ (eval t env)

  | S.Natelim { scrut; motive; case_zero; case_succ; } ->
     eval_nat_elim
       (eval scrut env)
       (close1 motive env)
       (eval case_zero env)
       (close2 case_succ env)

and eval_app v w =
  match v with
  | Lam c ->
     eval_clo1 c w
  | Reflect { ty = Forall (a, b); tm; } ->
     Reflect { ty = eval_clo1 b w; tm = App (tm, Reify { ty = a; tm = w; }); }
  | _ ->
     Error.internal "ill-typed evaluation"

and eval_nat_elim d m u0 uN =
  match d with
  | Zero ->
     u0
  | Succ n ->
     let vp = eval_nat_elim n m u0 uN in
     eval_clo2 uN n vp
  | Reflect { ty = Nat; tm; } ->
     Reflect { ty = Nat; tm = Natelim (tm, m, u0, uN); }
  | _ ->
     Error.internal "ill-typed evaluation"

and eval_clo1 (C1 (env, Bound1 { body; user; })) v =
  eval body (extend ?user v env)

and eval_clo2 (C2 (env, Bound2 { body; user1; user2; })) v1 v2 =
  eval body (extend ?user:user2 v2 (extend ?user:user1 v1 env))

(* {2 Quotation} *)

module Quote = struct
  module M = Monad.Reader(struct type t = int end)
  open Monad.Notation(M)

  let fresh ?n ty =
    let* v = fresh ty in
    return { n = Option.value ~default:Name.dummy n; v; ty = Some ty; }

  let (let$) : entry M.t -> (value -> 'a M.t) -> 'a M.t =
    fun x k free -> let en = x free in k en.v (free + 1)

  let rec neutral = function
    | Var lv ->
       let* free = M.get in
       return (S.Build.var (DeBruijn.ix_of_lv ~free lv))

    | App (ne, nf) ->
       let* ne = neutral ne in
       let* nf = normal nf in
       return @@ S.Build.app ne nf

    | Natelim (scrut, motive, case_zero, case_succ) ->
       let n = clo1_name motive in
       let* scrut = neutral scrut in
       let* case_zero = normal_ ~ty:(eval_clo1 motive Zero) ~tm:case_zero in
       let$ x1 = fresh ~n Nat in
       let* case_succ =
         let$ x2 = fresh ~n (eval_clo1 motive x1) in
         normal_clo2 ~ty:(eval_clo1 motive (Succ x1)) case_succ x1 x2
       in
       let* motive = typ_clo1 motive x1 in
       return @@ S.Build.natelim ~scrut ~motive ~case_zero ~case_succ ()

  and normal_ ~ty ~tm =
    match ty, tm with
    | _, Reflect { tm; _ } ->
       neutral tm

    | Type, _ ->
       typ tm

    | Forall (a, f), v ->
       let n = clo1_name f in
       let$ x = fresh ~n a in
       let* body = normal_ ~ty:(eval_clo1 f x) ~tm:(eval_app v x) in
       return @@ S.Build.lam (Bound1 { body; user = None; })

    | Nat, Zero ->
       return @@ S.Build.zero ()

    | Nat, Succ tm ->
       let* tm = normal_ ~ty ~tm in
       return @@ S.Build.succ tm

    | _, Lam _ ->
       Error.internal "eta-expansion failure"

    | _ ->
       Error.internal "ill-typed quotation"

  and normal (Reify { ty; tm; }) =
    normal_ ~ty ~tm

  and typ = function
    | Type ->
       return @@ S.Build.typ ()

    | Nat ->
       return @@ S.Build.nat ()

    | Forall (a, f) ->
       let n = clo1_name f in
       let* f =
         let$ x_a = fresh ~n a in
         typ_clo1 f x_a
       in
       let* a = typ a in
       return @@ S.Build.forall a f

    | Reflect _ | Zero | Succ _ | Lam _ ->
       Error.internal "ill-typed quotation"

  and typ_clo1 (C1 (_, Bound1 { user; _ }) as clo) x =
    let* body = typ (eval_clo1 clo x) in
    return @@ S.Bound1 { body; user; }

  and normal_clo2 ~ty (C2 (_, Bound2 { user1; user2; _ }) as clo) x1 x2 =
    let* body = normal_ ~ty ~tm:(eval_clo2 clo x1 x2) in
    return @@ S.Bound2 { body; user1; user2; }
end

(* {2 Normalization} *)

let nf ~env ~ty ~tm =
  let open Quote in
  M.run (Env.width env) (normal_ ~ty:(eval ty env) ~tm:(eval tm env))

(* {2 Type checking} *)

module M = struct
  include Monad.Reader(struct type t = env end)
  let run x = x Env.empty
end
open Monad.Notation(M)

(*   let (let$) : ty * value option -> *)

(* let (let$) : S.ty -> (value -> 'a M.t) -> 'a M.t = *)
(*   fun ty k free -> *)
(*   k (Reflect { ty; tm = Var (DeBruijn.Lv.last ~free); }) (free + 1) *)

let (let$) : entry M.t -> (value -> 'a M.t) -> 'a M.t =
  fun x k env -> let en = x env in k en.v (Env.extend en env)

let lift : 'a Quote.M.t -> 'a M.t = fun c env -> c (Env.width env)

let fresh ?n ?v ty =
  let* v =
    lift @@ match v with
            | None -> fresh ty
            | Some v -> Quote.M.return v
  in
  return { n = Option.value ~default:Name.dummy n; v; ty = Some ty; }

let check_conv ~expected ~actual loc =
  let* expected = lift @@ Quote.typ expected in
  let* actual = lift @@ Quote.typ actual in
  if not (Syntax.equal_term expected actual)
  then Error.unexpected_type ~expected ~actual loc;
  return ()

let rec check : expected:ty -> R.term -> S.term M.t =
  fun ~expected (Position.{ value = r; position = loc; } as tm) ->
  match r with
  | Let { def; ty; body; } ->
     let* ty = check_is_ty ty in
     let* tysem = eval ty in
     let* def = check ~expected:tysem def in
     let* defsem = eval def in
     let* body =
       let$ _ = fresh ~v:defsem tysem in
       check_bound1 ~expected body
     in
     return @@ S.Build.let_ ~loc ~def ~ty ~body ()

  | Forall _ ->
     assert false               (* TODO *)

  | Lam body ->
     begin match expected with
     | Forall (a, f) ->
        let$ x = fresh a in
        let* body = check_bound1 ~expected:(eval_clo1 f x) body in
        return @@ S.Build.lam ~loc body

     | actual ->
        let* actual = lift @@ Quote.typ actual in
        Error.unexpected_head_constr ~expected:`Forall ~actual loc
     end

  | Nat ->
     begin match expected with
     | Type ->
        return @@ S.Build.nat ~loc ()

     | actual ->
        let* actual = lift @@ Quote.typ actual in
        Error.unexpected_head_constr ~expected:`Univ ~actual loc
     end

  | Type ->
     begin match expected with
     | Type ->
        if !Options.type_in_type
        then return @@ S.Build.typ ~loc ()
        else Error.universe_inconsistency loc

     | actual ->
        let* actual = lift @@ Quote.typ actual in
        Error.unexpected_head_constr ~expected:`Univ ~actual loc
     end

  | Var _ | App _ | Zero | Succ _ | Natelim _ ->
     let* tm, actual = infer tm in
     let* () = check_conv ~expected ~actual loc in
     return tm

and check_is_ty : R.ty -> S.ty M.t =
  fun (Position.{ value = tm; position = loc; } as r) ->
  match tm with
  | Forall (a, f) ->
     let* a = check_is_ty a in
     let* f =
       let* asem = eval a in
       let$ _ = fresh asem in
       check_bound1_is_ty f
     in
     return @@ S.Build.forall ~loc a f

  | Nat ->
     return @@ S.Build.nat ~loc ()

  | Type ->
     return @@ S.Build.typ ~loc ()

  | Lam _ ->
     check ~expected:Type r

  | Let _ ->
     assert false               (* TODO *)

  | Var _ | App _ | Natelim _ | Zero | Succ _ ->
     let* tm, ty = infer r in
     begin match ty with
     | Type ->
        return tm
     | actual ->
        let* actual = lift @@ Quote.typ actual in
        Error.unexpected_head_constr ~expected:`Univ ~actual loc
     end

and infer : R.term -> (S.term * ty) M.t =
  fun Position.{ value = r; position = loc; } ->
  match r with
  | Var x ->
     let* ix, { ty; _ } = find loc x in
     return @@ (S.Build.var ix, Option.get ty)

  | App (m, n) ->
     let* m, mty = infer m in
     begin match mty with
     | Forall (a, f) ->
        let* n = check ~expected:a n in
        let* msem = eval m in
        return @@ (S.Build.app m n, eval_clo1 f msem)

     | actual ->
        let* actual = lift @@ Quote.typ actual in
        Error.unexpected_head_constr loc ~expected:`Forall ~actual
     end

  | R.Zero ->
     return @@ (S.Build.zero ~loc (), Nat)

  | R.Succ m ->
     let* m = check ~expected:Nat m in
     return @@ (S.Build.succ ~loc m, Nat)

  | Natelim { scrut; motive; case_zero; case_succ; } ->
     let* scrut = check ~expected:Nat scrut in
     let* motive =
       let$ _ = fresh Nat in
       check_bound1_is_ty motive
     in
     let* motsem = close1 motive in
     let* case_zero = check ~expected:(eval_clo1 motsem Zero) case_zero in
     let* case_succ =
       let$ x1 = fresh Nat in
       let$ _ = fresh (eval_clo1 motsem x1) in
       check_bound2 ~expected:(eval_clo1 motsem (Succ x1)) case_succ
     in
     let* resty =
       let* scrutsem = eval scrut in
       return @@ eval_clo1 motsem scrutsem
     in
     return @@ (S.Build.natelim ~scrut ~motive ~case_zero ~case_succ (), resty)

  | Let _ | Forall _ | Lam _ | Nat | Type ->
     Error.could_not_synthesize loc

and check_bound1_is_ty : R.bound1 -> S.bound1 M.t =
  fun (R.Bound1 { pat; body; }) ->
  let* body = check_is_ty body in
  return @@ S.Bound1 { user = Raw.name_option_of_pattern pat; body; }

and check_bound1 : expected:ty -> R.bound1 -> S.bound1 M.t =
  fun ~expected (R.Bound1 { pat; body; }) ->
  let* body = check ~expected body in
  return @@ S.Bound1 { user = Raw.name_option_of_pattern pat; body; }

and check_bound2 : expected:ty -> R.bound2 -> S.bound2 M.t =
  fun ~expected (R.Bound2 { pat1; pat2; body; }) ->
  let* body = check ~expected body in
  return @@ S.Bound2 { user1 = Raw.name_option_of_pattern pat1;
                       user2 = Raw.name_option_of_pattern pat2;
                       body; }

let phrase : R.phrase -> (S.phrase * env) M.t =
  fun Position.{ value; position = loc; } ->
  match value with
  | Val { name; ty; body; } ->
     let* ty = check_is_ty ty in
     let* tysem = eval ty in
     let* body = check ~expected:tysem body in
     let* env =
       let* bodysem = eval body in
       let$ _ = fresh ~n:name ~v:bodysem tysem in
       M.get
     in
     return @@ (S.Build.val_ ~loc ~user:name ~ty ~body (), env)

let rec check = function
  | [] ->
     return []
  | ph :: file ->
     let* ph, env = phrase ph in
     let file = check file env in
     return @@ ph :: file
