open Sexplib.Std

type cterm_desc =
  | Infer of iterm
  | Let of { def : cterm; ty : cterm; body : bound1; }
  | Pi of cterm * bound1
  | Lam of bound1
  | Sigma of cterm * bound1
  | Pair of cterm * cterm
  | Nat
  | Zero
  | Suc of cterm
  | UnitTy
  | Unit
  | Fin of cterm
  | Type of int

and cterm =
  {
    c_desc : cterm_desc;
    c_loc : Position.t;
    [@equal fun _ _ -> true]
    [@sexp_drop_if fun _ -> true]
  }

and iterm_desc =
  | Var of DeBruijn.Ix.t
  | App of iterm * cterm
  | Fst of iterm
  | Snd of iterm
  | Natelim of { scrut : cterm;
                 motive : bound1;
                 case_zero : cterm;
                 case_suc : bound2; }
  | Struct of { lv : int;
                scrut : cterm;
                body : bound1; }
  | Annot of { tm : cterm; ty : cterm; }

and iterm =
  {
    i_desc : iterm_desc;
    i_loc : Position.t;
    [@equal fun _ _ -> true]
    [@sexp_drop_if fun _ -> true]
  }

and bound1 =
  Bound1 of {
      body : cterm;
      user : Name.t option;
      [@equal fun _ _ -> true]
    }

and bound2 =
  Bound2 of {
      body : cterm;
      user1 : Name.t option;
      [@equal fun _ _ -> true]
        user2 : Name.t option;
      [@equal fun _ _ -> true]
    }

and phrase_desc =
  | Val of { user : Name.t option; ty : cterm; def : cterm; }
  | Eval of { def : iterm; }

and phrase =
  {
    ph_desc : phrase_desc;
    ph_loc : Position.t;
    [@equal fun _ _ -> true]
    [@sexp_drop_if fun _ -> true]
  }

and t = phrase list [@@deriving sexp_of, eq]

type ty = cterm

type telescope = (Name.t option * ty) list

module ToRaw = struct
  type env = Name.t DeBruijn.Env.t

  module M = Monad.Reader(struct type t = env end)
  open Monad.Notation(M)

  let lookup ix env = DeBruijn.Env.lookup env ix

  let (let$) p f env =
    let name =
      match p with
      | Some name -> name
      | None -> Name.internal @@ string_of_int @@ DeBruijn.Env.width env
    in
    f (Raw.Build.pvar ~name ()) (DeBruijn.Env.extend name env)

  let rec cterm { c_desc; c_loc; } =
    let* desc =
      match c_desc with
      | Infer tm ->
         let* tm = iterm tm in
         return @@ Position.value tm
      | Lam b ->
         let* b = bound1 b in
         return @@ Raw.Lam b
      | Pi (a, f) ->
         let* a = cterm a in
         let* f = bound1 f in
         return @@ Raw.Pi (a, f)
      | Sigma (a, f) ->
         let* a = cterm a in
         let* f = bound1 f in
         return @@ Raw.Sigma (a, f)
      | Pair (l, r) ->
         let* l = cterm l in
         let* r = cterm r in
         return @@ Raw.Pair (l, r)
      | Let { def; ty; body; } ->
         let* def = cterm def in
         let* ty = cterm ty in
         let* body = bound1 body in
         return @@ Raw.Let { def; ty; body; }
      | Zero ->
         return Raw.Zero
      | Suc t ->
         let* t = cterm t in
         return @@ Raw.Suc t
      | UnitTy ->
         return @@ Raw.UnitTy
      | Unit ->
         return @@ Raw.Unit
      | Fin sz ->
         let* sz = cterm sz in
         return @@ Raw.Fin sz
      | Type l ->
         return @@ Raw.Type l
      | Nat ->
         return Raw.Nat
    in
    return @@ Position.with_pos c_loc desc

  and iterm { i_desc; i_loc; } =
    let* desc =
      match i_desc with
      | Var ix ->
         let* x = DeBruijn.Env.lookup ix in
         return @@ Raw.Var x
      | App (t, u) ->
         let* t = iterm t in
         let* u = cterm u in
         return @@ Raw.App (t, u)
      | Fst m ->
         let* m = iterm m in
         return @@ Raw.Fst m
      | Snd m ->
         let* m = iterm m in
         return @@ Raw.Snd m
      | Natelim { scrut; motive; case_zero; case_suc; } ->
         let* scrut = cterm scrut in
         let* motive = bound1 motive in
         let* case_zero = cterm case_zero in
         let* case_suc = bound2 case_suc in
         return @@ Raw.Natelim { scrut; motive; case_zero; case_suc; }
      | Struct { lv; scrut; body; } ->
         let* scrut = cterm scrut in
         let* body = bound1 body in
         return @@ Raw.Struct { lv; scrut; body; }
      | Annot { tm; ty; } ->
         let* tm = cterm tm in
         let* ty = cterm ty in
         return @@ Raw.Annot { tm; ty; }
    in
    return @@ Position.with_pos i_loc desc

  and bound1 (Bound1 { body; user; }) =
    let$ pat = user in
    let* body = cterm body in
    return @@ Raw.Bound1 { pat; body; }

  and bound2 (Bound2 { body; user1; user2; }) =
    let$ pat1 = user1 in
    let$ pat2 = user2 in
    let* body = cterm body in
    return @@ Raw.Bound2 { pat1; pat2; body; }
  ;;

  let phrase { ph_desc; ph_loc; } =
    let* desc, env =
      match ph_desc with
      | Val { user; ty; def; } ->
         let name = Name.of_option user in
         let* ty = cterm ty in
         let* def = cterm def in
         let* env = M.get in
         return (Raw.Val { name; args = []; ty; def; },
                 DeBruijn.Env.extend name env)
      | Eval { def; } ->
         let* def = iterm def in
         let* env = M.get in
         return (Raw.Eval { def; }, env)
    in
    return @@ (Position.with_pos ph_loc desc, env)

  let file file env =
    let file, _ =
      List.fold_left
        (fun (file, env) ph -> let ph, env = phrase ph env in ph :: file, env)
        ([], env)
        file
    in
    List.rev file
end

module Build = struct
  let cdesc ?loc c_desc =
    { c_desc; c_loc = Option.value ~default:Position.dummy loc; }

  let idesc ?loc i_desc =
    { i_desc; i_loc = Option.value ~default:Position.dummy loc; }

  let infer ?loc tm = cdesc ?loc @@ Infer tm

  let let_ ?loc ~def ~ty ~body () = cdesc ?loc @@ Let { def; ty; body; }

  let bind_n mk ?loc tele body =
    List.fold_right
      (fun (user, a) body -> mk ?loc a (Bound1 { body; user; }))
      tele
      body

  let pi ?loc a f = cdesc ?loc @@ Pi (a, f)

  let pi_n ?loc tele body = bind_n pi ?loc tele body

  let lam ?loc bound = cdesc ?loc @@ Lam bound

  let sigma ?loc a f = cdesc ?loc @@ Sigma (a, f)

  let sigma_n ?loc tele body = bind_n sigma ?loc tele body

  let pair ?loc left right = cdesc ?loc @@ Pair (left, right)

  let nat ?loc () = cdesc ?loc Nat

  let unit_ty ?loc () =
    cdesc ?loc @@ UnitTy

  let unit ?loc () =
    cdesc ?loc @@ Unit

  let fin ?loc sz =
    cdesc ?loc @@ Fin sz

  let typ ?loc ~level () = cdesc ?loc @@ Type level

  let var ?loc ix = idesc ?loc @@ Var ix

  let app ?loc t u = idesc ?loc @@ App (t, u)

  let fst ?loc arg = idesc ?loc @@ Fst arg

  let snd ?loc arg = idesc ?loc @@ Snd arg

  let zero ?loc () = cdesc ?loc Zero

  let suc ?loc t = cdesc ?loc @@ Suc t

  let natelim ?loc ~scrut ~motive ~case_zero ~case_suc () =
    idesc ?loc @@ Natelim { scrut; motive; case_zero; case_suc; }

  let struct_ ?loc ~lv ~scrut ~body () =
    idesc ?loc @@ Struct { lv; scrut; body; }

  let annot ?loc ~tm ~ty () =
    idesc ?loc @@ Annot { tm; ty; }

  let val_ ?(loc = Position.dummy) ?user ~ty ~def () =
    { ph_loc = loc; ph_desc = Val { user; ty; def; } }

  let eval ?(loc = Position.dummy) ~def () =
    { ph_loc = loc; ph_desc = Eval { def; } }
end

module PPrint = struct
  let file file = Raw.PPrint.file (ToRaw.file file DeBruijn.Env.empty)
end
