open Sexplib.Conv

module R = Raw
module C = Core
module S = Semantics

module Env = DeBruijn.Env

(* {2 Utilities} *)

let bound1_name R.(Bound1 { pat; _ }) =
  R.name_of_pattern pat

let bound2_name_1 R.(Bound2 { pat1; _ }) =
  R.name_of_pattern pat1

let bound2_name_2 R.(Bound2 { pat2; _ }) =
  R.name_of_pattern pat2

(* {2 Type checking} *)

module M = struct
  include Monad.Reader(struct type t = S.env end)
  let run x = x Env.empty
end
open Monad.Notation(M)

let find loc x env =
  try Env.find (fun en -> en.S.user = x) env
  with Not_found -> Error.unbound_identifier loc x

let (let$) : S.entry M.t -> (S.value -> 'a M.t) -> 'a M.t =
  fun x k env -> let en = x env in k en.def (Env.extend en env)

let liftR : 'a C.ToRaw.M.t -> 'a M.t =
  fun x -> let* env = M.get in
           return @@ x (Env.map (fun S.{ user; _ } -> user) env)

let unexpected_type ~expected ~actual loc =
  let* expected = S.PPrint.value expected in
  let* actual = S.PPrint.value actual in
  Error.unexpected_type ~expected ~actual loc

let unexpected_head_constr ~expected ~actual loc =
  let* actual = S.PPrint.value actual in
  Error.unexpected_head_constr ~expected ~actual loc

let fresh ?def ~ty user =
  let open S.Quote in
  lift @@ fresh ~user ?def ty

let check_conv ~expected ~actual loc =
  let* conv = S.Quote.lift @@ S.Conv.ty expected actual in
  if conv then return () else unexpected_type ~expected ~actual loc

let rec check : expected:S.ty -> R.term -> C.term M.t =
  fun ~expected (Position.{ value = r; position = loc; } as tm) ->
  match r with
  | Let { def; ty; body; } ->
     let* ty = check_is_ty ty in
     let* tysem = S.Eval.term ty in
     let* def = check ~expected:tysem def in
     let* body =
       let* defsem = S.Eval.term def in
       let$ _ = fresh ~def:defsem ~ty:tysem @@ bound1_name body in
       check_bound1 ~expected body
     in
     return @@ C.Build.let_ ~loc ~def ~ty ~body ()

  | Forall (a, f) ->
     begin match expected with
     | S.Type ->
        let* a = check_is_ty a in
        let* f =
          let* asem = S.Eval.term a in
          let$ _ = fresh ~ty:asem (bound1_name f) in
          check_bound1_is_ty f
        in
        return @@ C.Build.forall ~loc a f

     | actual ->
        unexpected_head_constr ~expected:`Univ ~actual loc

     end

  | Lam body ->
     begin match expected with
     | Forall (a, f) ->
        let$ x = fresh ~ty:a @@ bound1_name body in
        let* body = check_bound1 ~expected:(S.Eval.clo1 f x) body in
        return @@ C.Build.lam ~loc body

     | actual ->
        unexpected_head_constr ~expected:`Forall ~actual loc
     end

  | Nat ->
     begin match expected with
     | Type ->
        return @@ C.Build.nat ~loc ()

     | actual ->
        unexpected_head_constr ~expected:`Univ ~actual loc
     end

  | Type ->
     begin match expected with
     | Type ->
        if !Options.type_in_type
        then return @@ C.Build.typ ~loc ()
        else Error.universe_inconsistency loc

     | actual ->
        unexpected_head_constr ~expected:`Univ ~actual loc
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
       let* asem = S.Eval.term a in
       let$ _ = fresh ~ty:asem @@ bound1_name f in
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
        unexpected_head_constr ~expected:`Univ ~actual loc
     end

and infer : R.term -> (C.term * S.ty) M.t =
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
        let* msem = S.Eval.term m in
        return @@ (C.Build.app m n, S.Eval.clo1 f msem)

     | actual ->
        unexpected_head_constr ~expected:`Forall ~actual loc
     end

  | R.Zero ->
     return @@ (C.Build.zero ~loc (), S.Nat)

  | R.Suc m ->
     let* m = check ~expected:Nat m in
     return @@ (C.Build.suc ~loc m, S.Nat)

  | Natelim { scrut; motive; case_zero; case_suc; } ->
     let* scrut = check ~expected:Nat scrut in
     let* motive =
       let$ _ = fresh ~ty:Nat @@ bound1_name motive in
       check_bound1_is_ty motive
     in
     let* motsem = S.close1 motive in
     let* case_zero = check ~expected:(S.Eval.clo1 motsem Zero) case_zero in
     let* case_suc =
       let$ x1 = fresh ~ty:Nat @@ bound2_name_1 case_suc in
       let$ _ = fresh ~ty:(S.Eval.clo1 motsem x1) @@ bound2_name_2 case_suc in
       check_bound2 ~expected:(S.Eval.clo1 motsem (Suc x1)) case_suc
     in
     let* resty =
       let* scrutsem = S.Eval.term scrut in
       return @@ S.Eval.clo1 motsem scrutsem
     in
     return @@ (C.Build.natelim ~scrut ~motive ~case_zero ~case_suc (), resty)

  | Let _ | Forall _ | Lam _ | Nat | Type ->
     Error.could_not_synthesize loc

and check_bound1_is_ty : R.bound1 -> C.bound1 M.t =
  fun (R.Bound1 { pat; body; }) ->
  let* body = check_is_ty body in
  return @@ C.Bound1 { user = Raw.name_option_of_pattern pat; body; }

and check_bound1 : expected:S.ty -> R.bound1 -> C.bound1 M.t =
  fun ~expected (R.Bound1 { pat; body; }) ->
  let* body = check ~expected body in
  return @@ C.Bound1 { user = Raw.name_option_of_pattern pat; body; }

and check_bound2 : expected:S.ty -> R.bound2 -> C.bound2 M.t =
  fun ~expected (R.Bound2 { pat1; pat2; body; }) ->
  let* body = check ~expected body in
  return @@ C.Bound2 { user1 = Raw.name_option_of_pattern pat1;
                       user2 = Raw.name_option_of_pattern pat2;
                       body; }

let phrase : R.phrase -> (C.phrase * S.env) M.t =
  fun Position.{ value; position = loc; } ->
  match value with
  | Val { name; ty; body; } ->
     let* ty = check_is_ty ty in
     let* tysem = S.Eval.term ty in
     let* body = check ~expected:tysem body in
     let* env =
       let* bodysem = S.Eval.term body in
       let$ _ = fresh ~def:bodysem ~ty:tysem name in
       M.get
     in
     return @@ (C.Build.val_ ~loc ~user:name ~ty ~body (), env)
  | Eval { body; ty; } ->
     let* ty = check_is_ty ty in
     let* tysem = S.Eval.term ty in
     let* body = check ~expected:tysem body in
     let* body = S.Conv.normalize ~ty:tysem ~tm:body in
     let* env = M.get in
     return @@ (C.Build.eval ~loc ~body ~ty (), env)

let rec check = function
  | [] ->
     return []
  | ph :: file ->
     let* ph, env = phrase ph in
     let file = check file env in
     return @@ ph :: file
