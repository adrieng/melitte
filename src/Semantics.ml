module C = Core
module E = DeBruijn.Env

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

and entry =
  {
    def : value;
    ty : value option;
    user : Name.t;
  }

and env = entry DeBruijn.Env.t

type ty = value

(* Utility functions *)

let clo1_name (C1 (_, C.Bound1 { user; _ })) =
  Option.value ~default:Name.dummy user

let close1 b1 env = C1 (env, b1)

let close2 b2 env = C2 (env, b2)

module Eval = struct
  module M = Monad.Reader(struct type t = env end)
  open Monad.Notation(M)

  let extend_eval ?(user = Name.dummy) def env =
    DeBruijn.Env.extend { def; ty = None; user; } env

  let rec term : C.term -> value M.t =
    fun t ->
    match t.t_desc with
    | C.Var ix ->
       begin
         try
           let* entry = DeBruijn.Env.lookup ix in
           return entry.def
         with Not_found -> Error.internal "ill-scoped evaluation"
       end

    | C.Lam body ->
       let* body = close1 body in
       return @@ Lam body

    | C.App (t, u) ->
       let* t = term t in
       let* u = term u in
       return @@ app t u

    | C.Forall (dom, cod) ->
       let* dom = term dom in
       let* cod = close1 cod in
       return @@ Forall (dom, cod)

    | C.Let { def; body; _ } ->
       let* def = term def in
       let* body = close1 body in
       return @@ clo1 body def

    | C.Type ->
       return Type

    | C.Nat ->
       return Nat

    | C.Zero ->
       return Zero

    | C.Suc t ->
       let* t = term t in
       return @@ Suc t

    | C.Natelim { scrut; motive; case_zero; case_suc; } ->
       let* scrut = term scrut in
       let* motive = close1 motive in
       let* case_zero = term case_zero in (* probably suboptimal *)
       let* case_suc = close2 case_suc in
       return @@ nat_elim scrut motive case_zero case_suc

  and app v w =
    match v with
    | Lam c ->
       clo1 c w
    | Reflect { ty = Forall (a, b); tm; } ->
       Reflect { ty = clo1 b w; tm = App (tm, Reify { ty = a; tm = w; }); }
    | _ ->
       Error.internal "ill-typed evaluation"

  and nat_elim d m u0 uN =
    match d with
    | Zero ->
       u0
    | Suc n ->
       let vp = nat_elim n m u0 uN in
       clo2 uN n vp
    | Reflect { ty = Nat; tm; } ->
       Reflect { ty = Nat; tm = Natelim (tm, m, u0, uN); }
    | _ ->
       Error.internal "ill-typed evaluation"

  and clo1 (C1 (env, Bound1 { body; user; })) v =
    term body (extend_eval ?user v env)

  and clo2 (C2 (env, Bound2 { body; user1; user2; })) v1 v2 =
    term body (extend_eval ?user:user2 v2 (extend_eval ?user:user1 v1 env))
end

module Quote = struct
  type state = { eta : bool; free : int; }
  module M = Monad.Reader(struct type t = state end)
  open Monad.Notation(M)

  let run ~eta ~free x = x { eta; free; }

  let lift : 'a M.t -> 'a Eval.M.t =
    fun x env -> run ~eta:true ~free:(DeBruijn.Env.width env) x

  let fresh ?(user = Name.dummy) ?def ty { free; _ } =
    let def =
      Option.value
        ~default:(Reflect { ty; tm = Var DeBruijn.Lv.(fresh ~free); })
        def
    in
    { user; def; ty = Some ty; }

  let (let$) : entry M.t -> (value -> 'a M.t) -> 'a M.t =
    fun x k state ->
    let en = x state in
    k en.def { state with free = state.free + 1; }

  let weaken : 'a M.t -> 'a M.t =
    fun x { eta; free; } -> x { eta; free = free + 1; }

  let rec neutral ne =
    match ne with
    | Var lv ->
       let* { free; _ } = M.get in
       return (C.Build.var (DeBruijn.ix_of_lv ~free lv))

    | App (ne, nf) ->
       let* ne = neutral ne in
       let* nf = normal nf in
       return @@ C.Build.app ne nf

    | Natelim (scrut, motive, case_zero, case_succ) ->
       let user = clo1_name motive in
       let* scrut = neutral scrut in
       let* case_zero = normal_ ~ty:(Eval.clo1 motive Zero) ~tm:case_zero in
       let$ x1 = fresh ~user Nat in
       let* case_suc =
         let$ x2 = fresh ~user (Eval.clo1 motive x1) in
         normal_clo2 ~ty:(Eval.clo1 motive (Suc x1)) case_succ x1 x2
       in
       let* motive = normal_clo1 ~ty:Type motive x1 in
       return @@ C.Build.natelim ~scrut ~motive ~case_zero ~case_suc ()

  and normal_eta ~ty ~tm =
    match ty, tm with
    | _, Reflect { tm; _ } ->
       neutral tm

    | Type, _ ->
       typ tm

    | Forall (a, f), v ->
       let user = clo1_name f in
       let$ x = fresh ~user a in
       let* body = normal_ ~ty:(Eval.clo1 f x) ~tm:(Eval.app v x) in
       return @@ C.Build.lam (Bound1 { body; user = None; })

    | Nat, Zero ->
       return @@ C.Build.zero ()

    | Nat, Suc tm ->
       let* tm = normal_ ~ty ~tm in
       return @@ C.Build.suc tm

    | _ ->
       Error.internal "ill-typed normal quotation"

  and normal_ ~ty ~tm =
    let* { eta; _ } = M.get in
    if eta then normal_eta ~ty ~tm else value tm

  and normal (Reify { ty; tm; }) =
    normal_ ~ty ~tm

  and typ = function
    | Type ->
       return @@ C.Build.typ ()

    | Nat ->
       return @@ C.Build.nat ()

    | Forall (a, f) ->
       let user = clo1_name f in
       let* f =
         let$ x_a = fresh ~user a in
         normal_clo1 ~ty:Type f x_a
       in
       let* a = typ a in
       return @@ C.Build.forall a f

    | Reflect { tm; _ } ->
       neutral tm

    | Zero ->
       Error.internal "ill-typed type quotation: zero"

    | Suc _ ->
       Error.internal "ill-typed type quotation: suc _"

    | Lam _ ->
       Error.internal "ill-typed type quotation: lam _"

  and normal_clo1 ~ty (C1 (_, Bound1 { user; _ }) as clo) x =
    let* body = normal_ ~ty ~tm:(Eval.clo1 clo x) in
    return @@ C.Bound1 { body; user; }

  and normal_clo2 ~ty (C2 (_, Bound2 { user1; user2; _ }) as clo) x1 x2 =
    let* body = normal_ ~ty ~tm:(Eval.clo2 clo x1 x2) in
    return @@ C.Bound2 { body; user1; user2; }

  (* This function avoids calling typ directly, since typ is type-directed and
     we might be acting on an ill-typed value here. *)
  and value = function
    | Reflect { tm; _ } ->
       neutral tm

    | Lam (C1 (env, body)) ->
       (* This could be written in direct style, but for the sake of consistency
          with the surrounding code I write in in monadic style. Having a
          generic monadic fold in DeBruijn.Env would remove the need for the
          explicit recurion here, but I believe that this is this is too
          unpleasant to write in current OCaml. *)
       let rec wrap_env envseq =
         match envseq () with
         | Seq.Nil -> return @@ C.Build.lam body
         | Seq.Cons ({ def; ty; user; }, envseq) ->
            let* def = value def in
            let* ty =
              match ty with
              | None -> return @@ C.Build.nat () (* dummy type *)
              | Some ty -> value ty
            in
            let* body = weaken @@ wrap_env envseq in
            return @@ C.Build.let_ ~def ~ty
                        ~body:(C.Bound1 { user = Some user; body; }) ()
       in
       return @@ run ~eta:false ~free:0 @@ wrap_env (DeBruijn.Env.to_seq env)

    | Forall (a, f) ->
       let user = clo1_name f in
       let* f =
         let$ x_a = fresh ~user a in
         (* We avoid calling typ_clo1 since this value might be ill-typed and we
            do not want the subsequent call to [quote_typ] to fail.*)
         normal_clo1 ~ty:Type f x_a
       in
       let* a = normal_ ~ty:Type ~tm:a in
       return @@ C.Build.forall a f

    | Type ->
       return @@ C.Build.typ ()

    | Nat ->
       return @@ C.Build.nat ()

    | Zero ->
       return @@ C.Build.zero ()

    | Suc tm ->
       let* tm = value tm in
       return @@ C.Build.suc tm
end

module Conv = struct
  let is_not_typ = function
    | Lam _ | Zero | Suc _ ->
       true
    | Reflect _ | Forall _ | Type | Nat ->
       false

  (* TODO implement smarter binary algorithm *)

  let ty v1 v2 =
    let open Monad.Notation(Quote.M) in
    if is_not_typ v1 || is_not_typ v2
    then return false
    else
      let* tm1 = Quote.typ v1 in
      let* tm2 = Quote.typ v2 in
      return @@ Core.equal_term tm1 tm2

  let normalize ~ty ~tm =
    let open Monad.Notation(Eval.M) in
    let* tm = Eval.term tm in
    let* tm = Quote.(lift @@ normal @@ Reify { tm; ty; }) in
    return tm
end

module PPrint = struct
  let value tm env =
    let tm =
      Quote.(run ~eta:false ~free:(DeBruijn.Env.width env) @@ value tm)
    in
    Core.ToRaw.term tm (DeBruijn.Env.map (fun { user; _ } -> user) env)
    |> Raw.PPrint.term

  let entry { def; ty; user; } env doc =
    let open PPrint in
    let ty = Option.fold ~none:(!^ "?") ~some:(fun v -> value v env) ty in
    group @@
      prefix 2 1
        (prefix 2 1 (!^ user ^^ colon) (ty ^^ space ^^ equals))
        (value def env)
      ^^ (if DeBruijn.Env.width env > 0 then semi ^^ space else empty)
      ^^ doc

  let env env =
    DeBruijn.Env.fold_cons entry env PPrint.empty
end
