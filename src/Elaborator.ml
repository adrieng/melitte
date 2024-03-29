module R = Raw
module C = Core
module S = Semantics
module L = UniverseLevel

module Env = DeBruijn.Env

(* {2 Utilities} *)

let bound1_name R.(Bound1 { pat; _ }) =
  R.name_of_pattern pat

let bound2_name_1 R.(Bound2 { pat1; _ }) =
  R.name_of_pattern pat1

let bound2_name_2 R.(Bound2 { pat2; _ }) =
  R.name_of_pattern pat2

(* {2 Type checking} *)

type state =
  {
    env : S.env;
    on_check_pre : Semantics.env -> expected:S.ty -> R.term -> unit;
    on_infer_pre : Semantics.env -> Raw.term -> unit;
    on_conversion_pre : Semantics.env ->
                        expected:Semantics.ty -> actual:Semantics.ty ->
                        Position.t -> unit;
    on_check_post : Semantics.env -> expected:S.ty -> R.term -> unit;
    on_infer_post : Semantics.env -> Raw.term -> actual:S.ty -> unit;
    on_conversion_post : Semantics.env ->
                         expected:Semantics.ty -> actual:Semantics.ty ->
                         Position.t -> unit;
  }

module M = struct
  include Monad.Reader(struct type t = state end)
end
open Monad.Notation(M)

let run
      ?(on_check_pre = fun _ ~expected _ -> ignore expected)
      ?(on_infer_pre = fun _ _ -> ())
      ?(on_conversion_pre = fun _ ~expected ~actual _ ->
                            ignore expected; ignore actual)
      ?(on_check_post = fun _ ~expected _ -> ignore expected)
      ?(on_infer_post = fun _ _ ~actual -> ignore actual)
      ?(on_conversion_post = fun _ ~expected ~actual _ ->
                             ignore expected; ignore actual)
      x =
  x {
      env = Env.empty;
      on_check_pre;
      on_infer_pre;
      on_conversion_pre;
      on_check_post;
      on_infer_post;
      on_conversion_post;
    }

let on_check_pre ~expected tm st =
  st.on_check_pre st.env ~expected tm

let on_infer_pre tm st =
  st.on_infer_pre st.env tm

let on_conversion_pre ~expected ~actual loc st =
  st.on_conversion_pre st.env ~expected ~actual loc

let on_check_post ~expected tm st =
  st.on_check_post st.env ~expected tm

let on_infer_post tm ~actual st =
  st.on_infer_post st.env tm ~actual

let on_conversion_post ~expected ~actual loc st =
  st.on_conversion_post st.env ~expected ~actual loc

let find loc x { env; _ } =
  try Env.find (fun en -> en.S.user = x) env
  with Not_found -> Error.unbound_identifier loc x

let (let$) : S.entry M.t -> (S.value -> 'a M.t) -> 'a M.t =
  fun x k st ->
  let en = x st in
  k en.def { st with env = Env.extend en st.env; }

let get_env { env; _ } = env

let liftE : 'a S.Eval.M.t -> 'a M.t =
  fun x st -> x st.env

let liftR : 'a C.ToRaw.M.t -> 'a M.t =
  fun x st -> x (Env.map (fun S.{ user; _ } -> user) st.env)

let incompatible_types ~expected ~actual loc =
  let* expected = liftE @@ S.PPrint.value expected in
  let* actual = liftE @@ S.PPrint.value actual in
  Error.incompatible_types ~expected ~actual loc

let unexpected_type ~expected loc =
  let* expected = liftE @@ S.PPrint.value expected in
  Error.unexpected_type ~expected loc

let unexpected_head_constr ~expected ~actual loc =
  let* actual = liftE @@ S.PPrint.value actual in
  Error.unexpected_head_constr ~expected ~actual loc

let quote_ty tysem =
  let open S.Quote in
  liftE @@ lift @@ typ tysem

let fresh ?def ~ty user =
  let open S.Quote in
  liftE @@ lift @@ fresh ~user ?def ty

let ceval tm = liftE @@ S.Eval.cterm tm
let ieval tm = liftE @@ S.Eval.iterm tm

let check_conv ~expected ~actual loc =
  let* () = on_conversion_pre ~expected ~actual loc in
  let* conv = liftE @@ S.Quote.lift @@ S.Conv.ty ~lo:actual ~hi:expected in
  if conv
  then on_conversion_post ~expected ~actual loc
  else incompatible_types ~expected ~actual loc

let rec check : expected:S.ty -> R.term -> C.cterm M.t =
  fun ~expected (Position.{ value = r; position = loc; } as tm) ->
  let* () = on_check_pre ~expected tm in
  match r with
  | Let { def; ty; body; } ->
     check_def ~name:(bound1_name body) ~ty ~def
       (fun ~ty ~def ->
         let* body = check_bound1 ~expected body in
         return @@ C.Build.let_ ~loc ~def ~ty ~body ())

  | Pi (a, f) | Sigma (a, f) ->
     let binder =
       match r with
       | Pi _ -> C.Build.pi | Sigma _ -> C.Build.sigma
       | _ -> assert false      (* absurd *)
     in
     begin match expected with
     | S.Type _ ->
        let* a = check ~expected a in
        let* f =
          let* asem = ceval a in
          let$ _ = fresh ~ty:asem (bound1_name f) in
          check_bound1 ~expected f
        in
        return @@ binder ~loc a f

     | actual ->
        unexpected_head_constr ~expected:`Univ ~actual loc
     end

  | Lam body ->
     begin match expected with
     | Pi (a, f) ->
        let$ x = fresh ~ty:a @@ bound1_name body in
        let* body = check_bound1 ~expected:(S.Eval.clo1 f x) body in
        return @@ C.Build.lam ~loc body

     | _ ->
        unexpected_type ~expected loc
     end

  | Pair (left, right) ->
     begin match expected with
     | Sigma (a, f) ->
        let* left = check ~expected:a left in
        let* right =
          let* leftsem = ceval left in
          check ~expected:(S.Eval.clo1 f leftsem) right
        in
        return @@ C.Build.pair ~loc left right

     | _ ->
        unexpected_type ~expected loc
     end

  | Nat ->
     begin match expected with
     | Type _ ->
        return @@ C.Build.nat ~loc ()

     | _ ->
        unexpected_type ~expected loc
     end

  | Type l_actual ->
     begin match expected with
     | Type l_expected ->
        if !Options.type_in_type || L.(fin l_actual <= l_expected)
        then return @@ C.Build.typ ~loc ~level:l_actual ()
        else Error.universe_inconsistency loc

     | _ ->
        unexpected_type ~expected loc

     end

  | Var _ | App _ | Zero | Suc _ | Natelim _ | Fst _ | Snd _ | Annot _ ->
     let* tm, actual = infer tm in
     let* () = check_conv ~expected ~actual loc in
     return @@ C.Build.infer ~loc tm

and check_is_ty : R.ty -> C.ty M.t =
  fun tm -> check ~expected:S.limtype tm

and infer : R.term -> (C.iterm * S.ty) M.t =
  fun (Position.{ value = r; position = loc; } as tm) ->
  let* () = on_infer_pre tm in
  let* tm', ty =
    match r with
    | Var x ->
       let* ix, { ty; _ } = find loc x in
       return @@ (C.Build.var ix, Option.get ty)

    | App (m, n) ->
       let* m, mty = infer m in
       begin match mty with
       | Pi (a, f) ->
          let* n = check ~expected:a n in
          let* nsem = ceval n in
          return @@ (C.Build.app ~loc m n, S.Eval.clo1 f nsem)

       | actual ->
          unexpected_head_constr ~expected:`Pi ~actual loc
       end

    | Fst m ->
       let* m, mty = infer m in
       begin match mty with
       | Sigma (a, _) ->
          return @@ (C.Build.fst ~loc m, a)

       | actual ->
          unexpected_head_constr ~expected:`Sigma ~actual m.C.i_loc
       end

    | Snd m ->
       let* m, mty = infer m in
       begin match mty with
       | Sigma (_, f) ->
          let* msem = ieval m in
          return @@ (C.Build.snd ~loc m, S.Eval.(clo1 f (fst msem)))

       | actual ->
          unexpected_head_constr ~expected:`Sigma ~actual m.C.i_loc
       end

    | Zero ->
        return @@ (C.Build.zero ~loc (), S.Nat)

    | Suc m ->
       let* m = check ~expected:S.Nat m in
       return @@ (C.Build.suc ~loc m, S.Nat)

    | Natelim { scrut; motive; case_zero; case_suc; } ->
       let* scrut = check ~expected:Nat scrut in
       let* motive =
         let$ _ = fresh ~ty:Nat @@ bound1_name motive in
         check_bound1_is_ty motive
       in
       let* motsem = liftE @@ S.close1 motive in
       let* case_zero = check ~expected:(S.Eval.clo1 motsem Zero) case_zero in
       let* case_suc =
         let$ x1 = fresh ~ty:Nat @@ bound2_name_1 case_suc in
         let$ _ = fresh ~ty:(S.Eval.clo1 motsem x1) @@ bound2_name_2 case_suc in
         check_bound2 ~expected:(S.Eval.clo1 motsem (Suc x1)) case_suc
       in
       let* resty =
         let* scrutsem = ceval scrut in
         return @@ S.Eval.clo1 motsem scrutsem
       in
       return @@ (C.Build.natelim ~loc ~scrut ~motive ~case_zero ~case_suc (),
                  resty)

    | Annot { tm; ty; } ->
       let* ty = check_is_ty ty in
       let* expected = ceval ty in
       let* tm = check ~expected tm in
       return @@ (C.Build.annot ~loc ~ty ~tm (), expected)

    | Let _ | Pi _ | Sigma _ | Lam _ | Pair _ | Nat | Type _ ->
       Error.could_not_synthesize loc
  in
  let* () = on_infer_post ~actual:ty tm in
  return (tm', ty)

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

and check_def : 'a. name:Name.t -> ?ty:R.ty -> def:R.term ->
                         (ty:C.ty -> def:C.cterm -> 'a M.t) -> 'a M.t =
  fun ~name ?ty ~def k ->
  let* ty, tysem, def =
    match ty with
    | Some ty ->
       let* ty = check_is_ty ty in
       let* tysem = ceval ty in
       let* def = check ~expected:tysem def in
       return (ty, tysem, def)
    | None ->
       let* def, tysem = infer def in
       let* ty = quote_ty tysem in
       return (ty, tysem, C.Build.infer def)
  in
  let* defsem = ceval def in
  let$ _ = fresh ~def:defsem ~ty:tysem name in
  k ~ty ~def

let phrase : R.phrase -> C.t M.t -> C.t M.t =
  fun Position.{ value; position = loc; } file ->
  match value with
  | Val { name; args; ty; def; } ->
     let ty = R.Build.pi_n ~loc ~params:args ~body:ty () in
     let def =
       R.Build.lam_n ~loc ~params:(R.patterns_of_telescope args) ~body:def ()
     in
     check_def ~name ~ty ~def
       (fun ~ty ~def ->
         let* file = file in
         return @@ C.Build.val_ ~loc ~user:name ~def ~ty () :: file)
  | Eval { def; } ->
     let* def, tysem = infer def in
     let* def = liftE @@ S.Conv.normalize ~ty:tysem ~tm:(C.Build.infer def) in
     let* ty = quote_ty tysem in
     let* file = file in
     return @@ C.Build.eval ~loc ~def:(C.Build.annot ~tm:def ~ty ()) () :: file

let rec check = function
  | [] ->
     return []
  | ph :: file ->
     phrase ph (check file)
