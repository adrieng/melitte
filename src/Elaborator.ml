open Sexplib.Conv

module R = Raw
module C = Core

module Env = DeBruijn.Env

type value =
  | Reflect of { ty : value; tm : neutral; }
  | Lam of clo1
  | Forall of value * clo1
  | Type
  | Nat
  | Zero
  | Suc of value

and neutral =
  | Var of DeBruijn.Lv.t
  | App of neutral * normal
  | Natelim of neutral * clo1 * value * clo2

and normal =
  | Reify of { ty : value; tm : value; }

and clo1 = C1 of env * C.bound1

and clo2 = C2 of env * C.bound2

(** Names are not used for evaluation since we only evaluate core syntax. Hence,
    environments are accessed via De Bruijn indices. *)
and env = entry Env.t

and entry = {
    v : value;                  (** Used for evaluation. *)
    ty : value option;          (** Used only during type checking, in which
                                    case this field is never [None]. *)
    n : Name.t;                 (** Used only for scoping during elaboration. *)
  }  [@@deriving sexp_of]

type ty = value

(* {2 Utilities} *)

let bound1_name R.(Bound1 { pat; _ }) =
  R.name_of_pattern pat

let bound2_name_1 R.(Bound2 { pat1; _ }) =
  R.name_of_pattern pat1

let bound2_name_2 R.(Bound2 { pat2; _ }) =
  R.name_of_pattern pat2

let clo1_name (C1 (_, C.Bound1 { user; _ })) =
  Option.value ~default:Name.dummy user

let fresh ty free =
  Reflect { ty; tm = Var DeBruijn.Lv.(fresh ~free); }

let find loc x env =
  try Env.find (fun en -> en.n = x) env
  with Not_found -> Error.unbound_identifier loc x

let extend ?ty ?user v env =
  Env.extend { v; ty; n = Option.value ~default:Name.dummy user; } env

(* {2 Evaluation} *)

let close1 b1 env = C1 (env, b1)

let close2 b2 env = C2 (env, b2)

let rec eval : C.term -> env -> value =
  fun t env ->
  match t.t_desc with
  | C.Var ix ->
     begin try (Env.lookup env ix).v
           with Not_found -> Error.internal "ill-scoped evaluation" end

  | C.Lam tm ->
     Lam (close1 tm env)

  | C.App (t, u) ->
     eval_app (eval t env) (eval u env)

  | C.Forall (a, b) ->
     Forall (eval a env, close1 b env)

  | C.Let { def; body; _ } ->
     eval_clo1 (close1 body env) (eval def env)

  | C.Type ->
     Type

  | C.Nat ->
     Nat

  | C.Zero ->
     Zero

  | C.Suc t ->
     Suc (eval t env)

  | C.Natelim { scrut; motive; case_zero; case_suc; } ->
     eval_nat_elim
       (eval scrut env)
       (close1 motive env)
       (eval case_zero env)
       (close2 case_suc env)

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
  | Suc n ->
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

let pp_env : env Sigs.Formatter.t =
  fun fmt env ->
  Sexplib.Sexp.pp_hum fmt @@ DeBruijn.Env.sexp_of_t sexp_of_entry env

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
       return (C.Build.var (DeBruijn.ix_of_lv ~free lv))

    | App (ne, nf) ->
       let* ne = neutral ne in
       let* nf = normal nf in
       return @@ C.Build.app ne nf

    | Natelim (scrut, motive, case_zero, case_succ) ->
       let n = clo1_name motive in
       let* scrut = neutral scrut in
       let* case_zero = normal_ ~ty:(eval_clo1 motive Zero) ~tm:case_zero in
       let$ x1 = fresh ~n Nat in
       let* case_suc =
         let$ x2 = fresh ~n (eval_clo1 motive x1) in
         normal_clo2 ~ty:(eval_clo1 motive (Suc x1)) case_succ x1 x2
       in
       let* motive = typ_clo1 motive x1 in
       return @@ C.Build.natelim ~scrut ~motive ~case_zero ~case_suc ()

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
       return @@ C.Build.lam (Bound1 { body; user = None; })

    | Nat, Zero ->
       return @@ C.Build.zero ()

    | Nat, Suc tm ->
       let* tm = normal_ ~ty ~tm in
       return @@ C.Build.suc tm

    | _, Lam _ ->
       Error.internal "eta-expansion failure"

    | _ ->
       Error.internal "ill-typed normal quotation"

  and normal (Reify { ty; tm; }) =
    normal_ ~ty ~tm

  and typ =
    fun t ->
    if !Options.verbose
    then Format.eprintf "quote %a@." Sexplib.Sexp.pp_hum (sexp_of_value t);
    match t with
    | Type ->
       return @@ C.Build.typ ()

    | Nat ->
       return @@ C.Build.nat ()

    | Forall (a, f) ->
       let n = clo1_name f in
       let* f =
         let$ x_a = fresh ~n a in
         typ_clo1 f x_a
       in
       let* a = typ a in
       return @@ C.Build.forall a f

    | Reflect { tm; _ } ->
       neutral tm

    | Zero | Suc _ | Lam _ ->
       Error.internal "ill-typed type quotation"

  and typ_clo1 (C1 (_, Bound1 { user; _ }) as clo) x =
    let* body = typ (eval_clo1 clo x) in
    return @@ C.Bound1 { body; user; }

  and normal_clo2 ~ty (C2 (_, Bound2 { user1; user2; _ }) as clo) x1 x2 =
    let* body = normal_ ~ty ~tm:(eval_clo2 clo x1 x2) in
    return @@ C.Bound2 { body; user1; user2; }
end

(* {2 Normalization} *)

(* {2 Type checking} *)

module M = struct
  include Monad.Reader(struct type t = env end)
  let run x = x Env.empty
end
open Monad.Notation(M)

let (let$) : entry M.t -> (value -> 'a M.t) -> 'a M.t =
  fun x k env -> let en = x env in k en.v (Env.extend en env)

let liftQ : 'a Quote.M.t -> 'a M.t =
  fun k env -> k (Env.width env)

let liftR : 'a C.ToRaw.M.t -> 'a M.t =
  fun k env -> k (Env.map (fun { n; _ } -> n) env)

let nf ~ty ~tm =
  let* tm = eval tm in
  let* ty = eval ty in
  liftQ @@ Quote.normal_ ~ty ~tm

let fresh ?v ty n =
  let* v = liftQ @@ match v with
                    | None -> fresh ty
                    | Some v -> Quote.M.return v
  in
  return { n; v; ty = Some ty; }

let raw_of_typ : value -> Raw.term M.t =
  fun tm ->
  let* tm = liftQ @@ Quote.typ tm in
  let* tm = liftR @@ Core.ToRaw.term tm in
  return tm

let check_conv ~expected ~actual loc =
  if !Options.verbose then
    Format.eprintf "@[%a@] =? @[%a@]@."
      Sexplib.Sexp.pp_hum (sexp_of_value expected)
      Sexplib.Sexp.pp_hum (sexp_of_value expected);
  let* expected = liftQ @@ Quote.typ expected in
  let* actual = liftQ @@ Quote.typ actual in
  if not (Core.equal_term expected actual)
  then
      let* expected = liftR @@ C.ToRaw.term expected in
      let* actual = liftR @@ C.ToRaw.term actual in
      Error.unexpected_type ~expected ~actual loc
  else
    return ()

let rec check : expected:ty -> R.term -> C.term M.t =
  fun ~expected (Position.{ value = r; position = loc; } as tm) ->
  match r with
  | Let { def; ty; body; } ->
     let* ty = check_is_ty ty in
     let* tysem = eval ty in
     let* def = check ~expected:tysem def in
     let* body =
       let* defsem = eval def in
       let$ _ = fresh ~v:defsem tysem (bound1_name body) in
       check_bound1 ~expected body
     in
     return @@ C.Build.let_ ~loc ~def ~ty ~body ()

  | Forall (a, f) ->
     begin match expected with
     | Type ->
        let* a = check_is_ty a in
        let* f =
          let* asem = eval a in
          let$ _ = fresh asem (bound1_name f) in
          check_bound1_is_ty f
        in
        return @@ C.Build.forall ~loc a f

     | actual ->
        let* actual = raw_of_typ actual in
        Error.unexpected_head_constr ~expected:`Univ ~actual loc

     end

  | Lam body ->
     begin match expected with
     | Forall (a, f) ->
        let$ x = fresh a (bound1_name body) in
        let* body = check_bound1 ~expected:(eval_clo1 f x) body in
        return @@ C.Build.lam ~loc body

     | actual ->
        let* actual = raw_of_typ actual in
        Error.unexpected_head_constr ~expected:`Forall ~actual loc
     end

  | Nat ->
     begin match expected with
     | Type ->
        return @@ C.Build.nat ~loc ()

     | actual ->
        let* actual = raw_of_typ actual in
        Error.unexpected_head_constr ~expected:`Univ ~actual loc
     end

  | Type ->
     begin match expected with
     | Type ->
        if !Options.type_in_type
        then return @@ C.Build.typ ~loc ()
        else Error.universe_inconsistency loc

     | actual ->
        let* actual = raw_of_typ actual in
        Error.unexpected_head_constr ~expected:`Univ ~actual loc
     end

  | Var _ | App _ | Zero | Suc _ | Natelim _ ->
     let* tm, actual = infer tm in
     let* () = check_conv ~expected ~actual loc in
     return tm

and check_is_ty : R.ty -> C.ty M.t =
  fun (Position.{ value = tm; position = loc; } as r) ->
  match tm with
  | Forall (a, f) ->
     let* a = check_is_ty a in
     let* f =
       let* asem = eval a in
       let$ _ = fresh asem (bound1_name f) in
       check_bound1_is_ty f
     in
     return @@ C.Build.forall ~loc a f

  | Nat ->
     return @@ C.Build.nat ~loc ()

  | Type ->
     return @@ C.Build.typ ~loc ()

  | Lam _ ->
     check ~expected:Type r

  | Let _ ->
     assert false               (* TODO *)

  | Var _ | App _ | Natelim _ | Zero | Suc _ ->
     let* tm, ty = infer r in
     begin match ty with
     | Type ->
        return tm
     | actual ->
        let* actual = raw_of_typ actual in
        Error.unexpected_head_constr ~expected:`Univ ~actual loc
     end

and infer : R.term -> (C.term * ty) M.t =
  fun Position.{ value = r; position = loc; } ->
  match r with
  | Var x ->
     let* ix, { ty; _ } = find loc x in
     return @@ (C.Build.var ix, Option.get ty)

  | App (m, n) ->
     let* m, mty = infer m in
     begin match mty with
     | Forall (a, f) ->
        let* n = check ~expected:a n in
        let* msem = eval m in
        return @@ (C.Build.app m n, eval_clo1 f msem)

     | actual ->
        let* actual = raw_of_typ actual in
        Error.unexpected_head_constr loc ~expected:`Forall ~actual
     end

  | R.Zero ->
     return @@ (C.Build.zero ~loc (), Nat)

  | R.Suc m ->
     let* m = check ~expected:Nat m in
     return @@ (C.Build.suc ~loc m, Nat)

  | Natelim { scrut; motive; case_zero; case_suc; } ->
     let* scrut = check ~expected:Nat scrut in
     let* motive =
       let$ _ = fresh Nat (bound1_name motive) in
       check_bound1_is_ty motive
     in
     let* motsem = close1 motive in
     let* case_zero = check ~expected:(eval_clo1 motsem Zero) case_zero in
     let* case_suc =
       let$ x1 = fresh Nat (bound2_name_1 case_suc) in
       let$ _ = fresh (eval_clo1 motsem x1) (bound2_name_2 case_suc) in
       check_bound2 ~expected:(eval_clo1 motsem (Suc x1)) case_suc
     in
     let* resty =
       let* scrutsem = eval scrut in
       return @@ eval_clo1 motsem scrutsem
     in
     return @@ (C.Build.natelim ~scrut ~motive ~case_zero ~case_suc (), resty)

  | Let _ | Forall _ | Lam _ | Nat | Type ->
     Error.could_not_synthesize loc

and check_bound1_is_ty : R.bound1 -> C.bound1 M.t =
  fun (R.Bound1 { pat; body; }) ->
  let* body = check_is_ty body in
  return @@ C.Bound1 { user = Raw.name_option_of_pattern pat; body; }

and check_bound1 : expected:ty -> R.bound1 -> C.bound1 M.t =
  fun ~expected (R.Bound1 { pat; body; }) ->
  let* body = check ~expected body in
  return @@ C.Bound1 { user = Raw.name_option_of_pattern pat; body; }

and check_bound2 : expected:ty -> R.bound2 -> C.bound2 M.t =
  fun ~expected (R.Bound2 { pat1; pat2; body; }) ->
  let* body = check ~expected body in
  return @@ C.Bound2 { user1 = Raw.name_option_of_pattern pat1;
                       user2 = Raw.name_option_of_pattern pat2;
                       body; }

let phrase : R.phrase -> (C.phrase * env) M.t =
  fun Position.{ value; position = loc; } ->
  match value with
  | Val { name; ty; body; } ->
     let* ty = check_is_ty ty in
     let* tysem = eval ty in
     let* body = check ~expected:tysem body in
     let* env =
       let* bodysem = eval body in
       let$ _ = fresh ~v:bodysem tysem name in
       M.get
     in
     return @@ (C.Build.val_ ~loc ~user:name ~ty ~body (), env)
  | Eval { body; ty; } ->
     let* ty = check_is_ty ty in
     let* body =
       let* tysem = eval ty in
       check ~expected:tysem body
     in
     let* body = nf ~ty ~tm:body in
     let* env = M.get in
     return @@ (C.Build.eval ~loc ~body ~ty (), env)

let rec check = function
  | [] ->
     return []
  | ph :: file ->
     let* ph, env = phrase ph in
     let file = check file env in
     return @@ ph :: file
