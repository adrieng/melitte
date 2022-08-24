open Sexplib.Conv

module C = Core
module E = DeBruijn.Env
module L = UniverseLevel

type value =
  | Reflect of { ty : value; tm : neutral; }
  | Lam of clo1
  | Pi of value * clo1
  | Sigma of value * clo1
  | Pair of value * value
  | Type of L.t
  | Nat
  | Zero
  | Suc of value

and neutral =
  | Var of DeBruijn.Lv.t
  | App of neutral * normal
  | Natelim of neutral * clo1 * value * clo2
  | Fst of neutral
  | Snd of neutral

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

and env = entry DeBruijn.Env.t [@@deriving sexp_of]

type ty = value

(* Utility functions *)

let clo1_name (C1 (_, C.Bound1 { user; _ })) =
  Option.value ~default:Name.dummy user

let close1 b1 env = C1 (env, b1)

let close2 b2 env = C2 (env, b2)

let limtype = Type UniverseLevel.inf

module Eval = struct
  module M = Monad.Reader(struct type t = env end)
  open Monad.Notation(M)

  let extend_eval ?(user = Name.dummy) def env =
    DeBruijn.Env.extend { def; ty = None; user; } env

  let rec cterm : C.cterm -> value M.t =
    fun tm ->
    match tm.c_desc with
    | C.Infer tm ->
       iterm tm

    | C.Lam body ->
       let* body = close1 body in
       return @@ Lam body

    | C.Pi (dom, cod) ->
       let* dom = cterm dom in
       let* cod = close1 cod in
       return @@ Pi (dom, cod)

    | C.Sigma (dom, cod) ->
       let* dom = cterm dom in
       let* cod = close1 cod in
       return @@ Sigma (dom, cod)

    | C.Pair (left, right) ->
       let* left = cterm left in
       let* right = cterm right in
       return @@ Pair (left, right)

    | C.Let { def; body; _ } ->
       let* def = cterm def in
       let* body = close1 body in
       return @@ clo1 body def

    | C.Type l ->
       return @@ Type (UniverseLevel.fin l)

    | C.Nat ->
       return Nat

    | C.Zero ->
       return Zero

    | C.Suc t ->
       let* t = cterm t in
       return @@ Suc t

  and iterm : C.iterm -> value M.t =
    fun tm ->
    match tm.i_desc with
    | C.Var ix ->
       begin
         try
           let* entry = DeBruijn.Env.lookup ix in
           return entry.def
         with Not_found -> Error.internal "ill-scoped evaluation"
       end

    | C.App (t, u) ->
       let* t = iterm t in
       let* u = cterm u in
       return @@ app t u

    | C.Fst t ->
       let* t = iterm t in
       return @@ fst t

    | C.Snd t ->
       let* t = iterm t in
       return @@ snd t

    | C.Natelim { scrut; motive; case_zero; case_suc; } ->
       let* scrut = cterm scrut in
       let* motive = close1 motive in
       let* case_zero = cterm case_zero in (* probably suboptimal *)
       let* case_suc = close2 case_suc in
       return @@ nat_elim scrut motive case_zero case_suc

    | C.Annot { tm; _ } ->
       cterm tm

  and app v w =
    match v with
    | Lam c ->
       clo1 c w
    | Reflect { ty = Pi (a, b); tm; } ->
       Reflect { ty = clo1 b w; tm = App (tm, Reify { ty = a; tm = w; }); }
    | _ ->
       Error.internal "ill-typed evaluation"

  and fst = function
    | Pair (l, _) ->
       l
    | Reflect { ty = Sigma (a, _); tm; } ->
       Reflect { ty = a; tm = Fst tm; }
    | _ ->
       Error.internal "ill-typed evaluation"

  and snd = function
    | Pair (_, r) ->
       r
    | Reflect { ty = Sigma (_, f); tm; } as tot ->
       Reflect { ty = clo1 f (fst tot); tm = Snd tm; }
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
    cterm body (extend_eval ?user v env)

  and clo2 (C2 (env, Bound2 { body; user1; user2; })) v1 v2 =
    cterm body (extend_eval ?user:user2 v2 (extend_eval ?user:user1 v1 env))
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

    | Fst ne ->
       let* ne = neutral ne in
       return @@ C.Build.fst ne

    | Snd ne ->
       let* ne = neutral ne in
       return @@ C.Build.snd ne

    | Natelim (scrut, motive, case_zero, case_succ) ->
       let user = clo1_name motive in
       let* scrut = neutral scrut in
       let* case_zero = normal_ ~ty:(Eval.clo1 motive Zero) ~tm:case_zero in
       let$ x1 = fresh ~user Nat in
       let* case_suc =
         let$ x2 = fresh ~user (Eval.clo1 motive x1) in
         normal_clo2 ~ty:(Eval.clo1 motive (Suc x1)) case_succ x1 x2
       in
       let* motive = normal_clo1 ~ty:(Type L.inf) motive x1 in
       return @@ C.Build.natelim
                   ~scrut:(C.Build.infer scrut)
                   ~motive ~case_zero ~case_suc ()

  and normal_eta ~ty ~tm =
    match ty, tm with
    | Reflect _, Reflect { tm; _ } ->
       let* tm = neutral tm in
       return @@ C.Build.infer tm

    | Type _, Type Inf ->
       Error.internal "limit universe quotation"

    | Type l1, Type (Fin level) ->
       if not !Options.type_in_type && L.(l1 <= fin level)
       then Error.internal "ill-typed normal quotation: universe level"
       else return @@ C.Build.typ ~level ()

    | Type _, Nat ->
       return @@ C.Build.nat ()

    | Type _, Pi (a, f) ->
       let user = clo1_name f in
       let* a' = typ a in
       let* f =
         let$ x_a = fresh ~user a in
         normal_clo1 ~ty f x_a
       in
       return @@ C.Build.pi a' f

    | Pi (a, f), _ ->
       let user = clo1_name f in
       let$ x = fresh ~user a in
       let* body = normal_ ~ty:(Eval.clo1 f x) ~tm:(Eval.app tm x) in
       return @@ C.Build.lam (Bound1 { body; user = None; })

    | Sigma (a, f), _ ->
       let base = Eval.fst tm in
       let* left = normal_ ~ty:a ~tm:base in
       let* right = normal_ ~ty:(Eval.clo1 f base) ~tm:(Eval.snd tm) in
       return @@ C.Build.pair left right

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

  and typ tm =
    normal_ ~ty:limtype ~tm

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
       let* tm = neutral tm in
       return @@ C.Build.infer tm

    | Lam (C1 (env, body)) ->
       (* This could be written in direct style, but for the sake of consistency
          uses monadic style. Having a generic monadic fold in DeBruijn.Env
          would remove the need for the explicit recurion here, but I believe
          that this is this is too unpleasant to write in current OCaml. *)
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

    | (Pi (a, f) | Sigma (a, f) as s) ->
       let binder =
         (* TODO refactor *)
         match s with
         | Pi _ -> C.Build.pi | Sigma _ -> C.Build.sigma
         | _ -> assert false    (* absurd *)
       in
       let user = clo1_name f in
       let* f = let$ x_a = fresh ~user a in normal_clo1 ~ty:limtype f x_a in
       let* a = normal_ ~ty:limtype ~tm:a in
       return @@ binder a f

    | Pair (left, right) ->
       let* left = value left in
       let* right = value right in
       return @@ C.Build.pair left right

    | Type (Fin level) ->
       return @@ C.Build.typ ~level ()

    | Type Inf ->
       return @@ C.Build.typ ~level:max_int ()
       (* Error.internal "limit universe quotation" *)

    | Nat ->
       return @@ C.Build.nat ()

    | Zero ->
       return @@ C.Build.zero ()

    | Suc tm ->
       let* tm = value tm in
       return @@ C.Build.suc tm
end

module Conv = struct
  open Monad.Notation(Quote.M)

  (* TODO factor out somehow *)
  let (let$) = Quote.(let$)

  let (&&&) x y = let* b = x in if b then y else return false

  let level ~allow_subtype ~lo ~hi =
    let open UniverseLevel in
    return @@ if allow_subtype then lo <= hi else lo = hi

  let rec normal_ ~allow_subtype ~ty ~lo ~hi =
    match ty, lo, hi with
    | _,
      Reflect { tm = lo; _ },
      Reflect { tm = hi; _ } ->
       neutral ~allow_subtype ~lo ~hi

    | Type _,
      Nat,
      Nat ->
       return true

    | Type _,
      Type l_lo,
      Type l_hi ->
       level ~allow_subtype ~lo:l_lo ~hi:l_hi

    | Type _,
      Pi (lo_dom, lo_cod),
      Pi (hi_dom, hi_cod) ->
       binder1 ~allow_subtype ~ty ~lo_dom ~lo_cod ~hi_dom ~hi_cod

    | Type _,
      Sigma (lo_dom, lo_cod),
      Sigma (hi_dom, hi_cod) ->
       binder1 ~allow_subtype ~ty ~lo_dom ~lo_cod ~hi_dom ~hi_cod

    | Nat,
      Zero,
      Zero ->
       return true

    | Nat,
      Suc lo,
      Suc hi ->
       normal_ ~allow_subtype ~ty ~lo ~hi

    | Pi (dom, cod),
      _,
      _ ->
       let$ x = Quote.fresh ~user:(clo1_name cod) dom in
       normal_
         ~allow_subtype
         ~ty:(Eval.clo1 cod x)
         ~lo:(Eval.app lo x)
         ~hi:(Eval.app hi x)

    | Sigma (dom, cod),
      _,
      _ ->
       normal_ ~allow_subtype ~ty:dom ~lo:(Eval.fst lo) ~hi:(Eval.fst hi)
       &&& normal_ ~allow_subtype
             ~ty:(Eval.clo1 cod (Eval.fst lo))
             ~lo:(Eval.snd lo)
             ~hi:(Eval.snd hi)

    | _ ->
       return false

  and binder1 ~allow_subtype ~ty ~lo_dom ~lo_cod ~hi_dom ~hi_cod =
    normal_ ~allow_subtype ~ty ~lo:hi_dom ~hi:lo_dom
    &&&
      let$ x = Quote.fresh ~user:(clo1_name lo_cod) lo_dom in
      normal_ ~allow_subtype
        ~ty
        ~lo:(Eval.clo1 lo_cod x)
        ~hi:(Eval.clo1 hi_cod x)

  and normal ~allow_subtype ~lo ~hi =
    let Reify { tm = lo; ty = lo_ty; } = lo in
    let Reify { tm = hi; ty = hi_ty; } = hi in
    normal_ ~allow_subtype ~ty:limtype ~lo:lo_ty ~hi:hi_ty
    &&& normal_ ~allow_subtype ~ty:lo ~lo ~hi

  and neutral ~allow_subtype ~lo ~hi =
    match lo, hi with
    | Var lv1,
      Var lv2 ->
       return @@ DeBruijn.Lv.equal lv1 lv2

    | App (lo_ne, lo_nf),
      App (hi_ne, hi_nf) ->
       neutral ~allow_subtype ~lo:lo_ne ~hi:hi_ne
       &&& normal ~allow_subtype ~lo:lo_nf ~hi:hi_nf

    | Natelim (lo_scrut, lo_motive, lo_case_zero, lo_case_succ),
      Natelim (hi_scrut, hi_motive, hi_case_zero, hi_case_succ) ->
       neutral ~allow_subtype ~lo:lo_scrut ~hi:hi_scrut
       &&& (let$ x1 = Quote.fresh ~user:(clo1_name lo_motive) Nat in
            normal_clo1 ~allow_subtype
              ~ty:limtype ~lo:lo_motive ~hi:hi_motive x1)
       &&& normal_ ~allow_subtype
             ~ty:(Eval.clo1 lo_motive Zero) ~lo:lo_case_zero ~hi:hi_case_zero
       &&&
         let$ x1 = Quote.fresh ~user:(clo1_name lo_motive) Nat in
         let$ x2 =
           Quote.fresh ~user:(clo1_name lo_motive) (Eval.clo1 lo_motive x1)
         in
         normal_clo2
           ~allow_subtype
           ~ty:(Eval.clo1 lo_motive (Suc x1))
           ~lo:lo_case_succ ~hi:hi_case_succ x1 x2

    | _ ->
       return false

  and normal_clo1 ~allow_subtype ~ty ~lo ~hi arg =
    normal_ ~allow_subtype ~ty ~lo:(Eval.clo1 lo arg) ~hi:(Eval.clo1 hi arg)

  and normal_clo2 ~allow_subtype ~ty ~lo ~hi arg1 arg2 =
    normal_ ~allow_subtype ~ty
      ~lo:(Eval.clo2 lo arg1 arg2)
      ~hi:(Eval.clo2 hi arg1 arg2)

  let ty ~lo ~hi =
    normal_ ~allow_subtype:true ~ty:limtype ~lo ~hi

  let normalize ~ty ~tm =
    let open Monad.Notation(Eval.M) in
    let* tm = Eval.cterm tm in
    let* tm = Quote.(lift @@ normal @@ Reify { tm; ty; }) in
    return tm
end

module PPrint = struct
  let value tm env =
    let tm =
      Quote.(run ~eta:false ~free:(DeBruijn.Env.width env) @@ value tm)
    in
    Core.ToRaw.cterm tm (DeBruijn.Env.map (fun { user; _ } -> user) env)
    |> Raw.PPrint.term

  let entry { def; ty; user; } env doc =
    let open PPrint in
    let ty = match ty with
      | None -> empty
      | Some ty -> group @@ colon ^/^ value ty env ^^ space
    in
    group @@
      prefix 2 1
        (prefix 2 1 (Name.pp user) (ty ^^ equals))
        (value def env)
      ^^ (if DeBruijn.Env.width env > 1 then semi ^^ space else empty)
      ^^ doc

  let env env =
    DeBruijn.Env.fold_cons entry env PPrint.empty

  let clo1 (C1 (cenv, bound1)) =
    let doc =
      Core.ToRaw.bound1 bound1 (E.map (fun { user; _ } -> user) cenv)
      |> Raw.PPrint.bound1
    in
    PPrint.(doc ^^ braces (env cenv))

  let clo2 (C2 (cenv, bound2)) =
    let doc =
      Core.ToRaw.bound2 bound2 (E.map (fun { user; _ } -> user) cenv)
      |> Raw.PPrint.bound2
    in
    PPrint.(doc ^^ braces (env cenv))
end
