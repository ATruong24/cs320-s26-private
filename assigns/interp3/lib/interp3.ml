open Utils
module Error_msg = Error_msg
module Ast = Ast

(* SYNTAX
   ----------------------------------------------------------------------
*)

type ty = Ast.Type.t =
  | TUnit
  | TBool
  | TInt
  | TString
  | TTuple of ty list
  | TAdt of ty list * string
  | TFun of ty * ty
  | TParam of string

type _pattern = Ast.Pattern.pattern =
  | PWild
  | PVar of string
  | PUnit
  | PBool of bool
  | PInt of int
  | PString of string
  | PTuple of pattern list
  | PCons of string * pattern option
and pattern = Ast.Pattern.t =
  {
    pos : pos;
    pattern : _pattern;
  }

type bop = Ast.Expr.bop =
  | Add | Sub | Mul
  | Div | Mod
  | And | Or
  | Concat
  | Eq | Neq | Lt | Lte | Gt | Gte

type _expr = Ast.Expr.expr =
  | Unit
  | Bool of bool
  | Int of int
  | String of string
  | Negate of expr
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | Annot of expr * ty
  | Tuple of expr list
  | Assert of expr
  | Var of string
  | Cons of string * expr option
  | Fun of (string * ty option) * expr
  | App of expr * expr
  | Let of
      {
        is_rec : bool;
        name : string;
        binding : expr;
        body : expr;
      }
  | Match of expr * (pattern * expr) list
and expr = Ast.Expr.t =
  {
    pos : pos;
    expr : _expr;
  }

type _stmt = Ast.Stmt.stmt =
  | SLet of
      {
        is_rec : bool;
        name : string;
        binding : expr;
      }

  | SAdt of
      {
        tpars : string list;
        name : string;
        constrs : (string * ty option) list
      }
and stmt = Ast.Stmt.t =
  {
    pos : pos;
    stmt : _stmt;
  }

module Env = Map.Make(String)


(* TYPE ERRORS
   ----------------------------------------------------------------------
*)

let dummy_error = Error_msg.mk dummy_pos "Dummy error"

let unknown_var pos x = Error_msg.mk pos (Format.asprintf "Unbound value %s" x)

let exp_ty pos t1 t2 =
  let msg =
    Format.asprintf
      "This expression has type %a but an expression was expected of type %a"
      Ast.Type.pp t1 Ast.Type.pp t2
  in Error_msg.mk pos msg

let invalid_app pos = Error_msg.mk pos "Invalid application"

let invalid_tuple pos = Error_msg.mk pos "Invalid tuple"

let unknown_cons pos x = Error_msg.mk pos (Format.asprintf "Unbound constructor %s" x)

let cons_exp_no_args pos x =
  Error_msg.mk
    pos
    (Format.asprintf "The constructor %s expects 0 arguments" x)

let cons_exp_args pos x =
  Error_msg.mk
    pos
    (Format.asprintf "The constructor %s expects arguments" x)

let exp_pat pos t1 t2 =
  let msg =
    Format.asprintf
      "This pattern matches values of type %a but a pattern was expected which matches values of type %a"
      Ast.Type.pp t1 Ast.Type.pp t2
  in Error_msg.mk pos msg

let bound_several_times pos x =
  let msg =
    Format.asprintf
      "Variable %s is bound several times in this matching"
      x
  in Error_msg.mk pos msg

let dup_ty_name pos x =
  let msg =
    Format.asprintf
      "Type using name %s is already defined"
      x
  in Error_msg.mk pos msg

let unbound_ty_var pos n =
  Error_msg.mk
    pos
    (Format.asprintf "The type variable %s is unbound in this type declaration" n)

let ty_param_several_times pos =
  Error_msg.mk
    pos
    "A type parameter occurs several times"

(* TYPING
   ----------------------------------------------------------------------
*)

type ty_scheme = string list * ty
type ctxt = ty_scheme Env.t
type constr = ty * ty
type solution = (string * ty) list
let fresh () = TParam(_gensym ())

let add x ty ctxt = Env.add x ([], ty) ctxt


let rec nub l =
  match l with
  | [] -> []
  | x :: xs -> x :: List.filter ((<>) x) (nub xs)

let free_vars ty =
  let rec go = function
    | TUnit | TBool | TInt | TString -> []
    | TTuple ts -> List.concat (List.map go ts)
    | TAdt (ts, _) -> List.concat (List.map go ts)
    | TFun (t1, t2) -> go t1 @ go t2
    | TParam a -> [a]
  in
  nub (go ty)

let is_free a t = 
  let rec go = function
    | TUnit | TBool | TInt | TString -> false
    | TTuple ts | TAdt (ts, _) -> List.exists (go) ts
    | TFun (t1, t2) -> go t1 || go t2
    | TParam b -> a = b
  in go t

let subst t1 a t2 =
  let rec go = function
    | TInt -> TInt
    | TBool -> TBool
    | TUnit -> TUnit
    | TString -> TString
    | TTuple ts -> TTuple (List.map go ts)
    | TAdt (ts, name) -> TAdt (List.map go ts, name)
    | TFun (t1, t2) -> TFun (go t1, go t2)
    | TParam b -> 
      if a = b 
        then t1 
      else 
        TParam b
    in go t2
  
  let instantiate (vars, ty) =
    List.fold_left (fun ty a -> subst (fresh ()) a ty) ty vars

  let unify (cs : constr list) : (solution, ty * ty) result =
  let rec loop accS cs =
    match cs with
    | [] -> Ok accS
    | eq :: xcs ->
      begin
        match eq with
        | (t1, t2) when t1 = t2 -> loop accS xcs
        | (TFun (s1, t1), TFun (s2, t2)) ->
          loop accS ((s1, s2) :: (t1, t2) :: xcs)
        | (TTuple ts1, TTuple ts2) when List.length ts1 = List.length ts2 ->
          loop accS (List.combine ts1 ts2 @ xcs)
        | (TAdt (ts1, n1), TAdt (ts2, n2))
          when n1 = n2 && List.length ts1 = List.length ts2 ->
          loop accS (List.combine ts1 ts2 @ xcs)
        | (TParam a, t) when not (is_free a t) ->
          let newS =
            (a, t) :: List.map (fun (v, ty) -> (v, subst t a ty)) accS
          in
          let subU =
            List.map (fun (t1, t2) -> subst t a t1, subst t a t2) xcs
          in
          loop newS subU
        | (t, TParam a) ->
          loop accS ((TParam a, t) :: xcs)
        | (t1, t2) -> Error (t1, t2)
      end
  in
  loop [] cs

  let name_of_num n =
  let c = Char.chr (Char.code 'a' + (n mod 26)) in
  if n < 26
  then String.make 1 c
  else String.make 1 c ^ string_of_int (n / 26)

let principal (sol : solution) (ty : ty) : ty_scheme =
  let ty =
    List.fold_left
      (fun ty (a, t) -> subst t a ty)
      ty
      sol
  in
  let vars = free_vars ty in
  let rec make_names n vars =
    match vars with
    | [] -> []
    | _ :: xs -> name_of_num n :: make_names (n + 1) xs
  in
  let names = make_names 0 vars in
  let ty =
    List.fold_left2
      (fun ty old_name new_name -> subst (TParam new_name) old_name ty)
      ty
      vars
      names
  in
  names, ty

let check_disjoint_two (ctxt1 : ctxt) (ctxt2 : ctxt) : string option =
  Env.fold
    (fun name _ acc ->
      match acc with
      | Some _ -> acc
      | None ->
        if name <> "_" && Env.mem name ctxt2
        then Some name
        else None)
    ctxt1
    None

let check_disjoint (ctxt_lst : ctxt list) : (unit, string) result =
  let rec loop acc ctxt_lst =
    match ctxt_lst with
    | [] -> Ok ()
    | curr_ctxt :: rest ->
      begin
        match check_disjoint_two curr_ctxt acc with
        | Some dup_name -> Error dup_name
        | None ->
          let new_ctxt =
            Env.fold
              (fun key value acc -> Env.add key value acc)
              curr_ctxt
              acc
          in
          loop new_ctxt rest
      end
  in
  loop Env.empty ctxt_lst

let merge_ctxts (ctxt_lst : ctxt list) : ctxt =
  let merge_two env1 env2 =
    Env.fold
      (fun key value acc -> Env.add key value acc)
      env2
      env1
  in
  List.fold_left merge_two Env.empty ctxt_lst

let split3 (lst : (ty * constr list * ctxt) list) =
  let rec loop acc1 acc2 acc3 lst =
    match lst with
    | [] -> List.rev acc1, List.rev acc2, List.rev acc3
    | (ty, constr, ctxt) :: xs ->
      loop (ty :: acc1) (constr :: acc2) (ctxt :: acc3) xs
  in
  loop [] [] [] lst

  let rec constrs_of_pattern (ctxt : ctxt) (p : pattern) : (ty * constr list * ctxt, Error_msg.t) result =
  match p.pattern with
  | PWild ->
    let a = fresh () in
    Ok (a, [], Env.empty)
  | PVar x ->
    let a = fresh () in
    Ok (a, [], Env.add x ([], a) Env.empty)
  | PUnit -> Ok (TUnit, [], Env.empty)
  | PBool _ -> Ok (TBool, [], Env.empty)
  | PInt _ -> Ok (TInt, [], Env.empty)
  | PString _ -> Ok (TString, [], Env.empty)
  | PTuple p_lst ->
    if List.length p_lst < 2
    then Error (invalid_tuple p.pos)
    else
      let rec process_patterns lst =
        match lst with
        | [] -> Ok []
        | p :: ps ->
          begin
            match constrs_of_pattern ctxt p with
            | Error e -> Error e
            | Ok current_res ->
              begin
                match process_patterns ps with
                | Error e -> Error e
                | Ok rest_res -> Ok (current_res :: rest_res)
              end
          end
      in
      begin
        match process_patterns p_lst with
        | Error e -> Error e
        | Ok res_lst ->
          let ty_lst, constr_lst, ctxt_lst = split3 res_lst in
          begin
            match check_disjoint ctxt_lst with
            | Error dup_name -> Error (bound_several_times p.pos dup_name)
            | Ok () ->
              let final_ctxt = merge_ctxts ctxt_lst in
              let final_constrs = List.concat constr_lst in
              Ok (TTuple ty_lst, final_constrs, final_ctxt)
          end
      end
  | PCons (c_name, pat_opt) ->
    begin
      match Env.find_opt c_name ctxt with
      | None -> Error (unknown_cons p.pos c_name)
      | Some (alphas, c_ty) ->
        let betas = List.map (fun _ -> fresh ()) alphas in
        let replaced_vars =
          List.fold_left2
            (fun acc_ty beta alpha -> subst beta alpha acc_ty)
            c_ty
            betas
            alphas
        in
        begin
          match pat_opt, replaced_vars with
          | None, TAdt _ -> Ok (replaced_vars, [], Env.empty)
          | Some inner_pat, TFun (sigma, adt_return_ty) ->
            begin
              match constrs_of_pattern ctxt inner_pat with
              | Error e -> Error e
              | Ok (pat_ty, pat_cs, pat_ctxt) ->
                let final_constrs = (pat_ty, sigma) :: pat_cs in
                Ok (adt_return_ty, final_constrs, pat_ctxt)
            end
          | None, TFun _ -> Error (cons_exp_args p.pos c_name)
          | Some _, TAdt _ -> Error (cons_exp_no_args p.pos c_name)
          | _ -> assert false
        end
    end

let rec constrs_of (ctxt : ctxt) (e : expr) : (ty * constr list, Error_msg.t) result =
  match e.expr with
  | Unit -> Ok (TUnit, [])
  | Bool _ -> Ok (TBool, [])
  | Int _ -> Ok (TInt, [])
  | String _ -> Ok (TString, [])
  | Negate e1 ->
    begin
      match constrs_of ctxt e1 with
      | Error e -> Error e
      | Ok (t1, cs1) -> Ok (TInt, (t1, TInt) :: cs1)
    end
  | Bop (bop, e1, e2) ->
    begin
      match constrs_of ctxt e1 with
      | Error e -> Error e
      | Ok (t1, cs1) ->
        begin
          match constrs_of ctxt e2 with
          | Error e -> Error e
          | Ok (t2, cs2) ->
            begin
              match bop with
              | Add | Sub | Mul | Div | Mod -> Ok (TInt, [(t1, TInt); (t2, TInt)] @ cs1 @ cs2)
              | And | Or -> Ok (TBool, [(t1, TBool); (t2, TBool)] @ cs1 @ cs2)
              | Concat -> Ok (TString, [(t1, TString); (t2, TString)] @ cs1 @ cs2)
              | Eq | Neq | Lt | Lte | Gt | Gte -> Ok (TBool, (t1, t2) :: cs1 @ cs2)
            end
        end
    end
  | If (e1, e2, e3) ->
    begin
      match constrs_of ctxt e1 with
      | Error e -> Error e
      | Ok (t1, cs1) ->
        begin
          match constrs_of ctxt e2 with
          | Error e -> Error e
          | Ok (t2, cs2) ->
            begin
              match constrs_of ctxt e3 with
              | Error e -> Error e
              | Ok (t3, cs3) ->
                Ok (t2, [(t1, TBool); (t2, t3)] @ cs1 @ cs2 @ cs3)
            end
        end
    end
  | Annot (e1, ty) ->
    begin
      match constrs_of ctxt e1 with
      | Error e -> Error e
      | Ok (t1, cs1) -> Ok (ty, (t1, ty) :: cs1)
    end
  | Tuple e_lst ->
    if List.length e_lst < 2
    then Error (invalid_tuple e.pos)
    else
      let rec process_exprs lst =
        match lst with
        | [] -> Ok []
        | e :: es ->
          begin
            match constrs_of ctxt e with
            | Error e -> Error e
            | Ok current_res ->
              begin
                match process_exprs es with
                | Error e -> Error e
                | Ok rest_res -> Ok (current_res :: rest_res)
              end
          end
      in
      begin
        match process_exprs e_lst with
        | Error e -> Error e
        | Ok res_lst ->
          let ty_lst, cs_lst = List.split res_lst in
          Ok (TTuple ty_lst, List.concat cs_lst)
      end
  | Assert {expr = Bool false; _} ->
    let a = fresh () in
    Ok (a, [])
  | Assert e1 ->
    begin
      match constrs_of ctxt e1 with
      | Error e -> Error e
      | Ok (t1, cs1) -> Ok (TUnit, (t1, TBool) :: cs1)
    end

  | Var x ->
    begin
      match Env.find_opt x ctxt with
      | None -> Error (unknown_var e.pos x)
      | Some (vars, ty) ->
        let ty =
          List.fold_left
            (fun ty a -> subst (fresh ()) a ty)
            ty
            vars
        in
        Ok (ty, [])
    end
  | Cons (c_name, e_opt) ->
    begin
      match Env.find_opt c_name ctxt with
      | None -> Error (unknown_cons e.pos c_name)
      | Some (alphas, c_ty) ->
        let betas = List.map (fun _ -> fresh ()) alphas in
        let replaced_vars =
          List.fold_left2
            (fun acc_ty beta alpha -> subst beta alpha acc_ty)
            c_ty
            betas
            alphas
        in
        begin
          match e_opt, replaced_vars with
          | None, TAdt _ -> Ok (replaced_vars, [])
          | Some e1, TFun (sigma, adt_return_ty) ->
            begin
              match constrs_of ctxt e1 with
              | Error e -> Error e
              | Ok (t1, cs1) -> Ok (adt_return_ty, (t1, sigma) :: cs1)
            end
          | None, TFun _ -> Error (cons_exp_args e.pos c_name)
          | Some _, TAdt _ -> Error (cons_exp_no_args e.pos c_name)
          | _ -> assert false
        end
    end
  | Fun ((x, ty_opt), body) ->
    let arg_ty =
      match ty_opt with
      | None -> fresh ()
      | Some ty -> ty
    in
    begin
      match constrs_of (add x arg_ty ctxt) body with
      | Error e -> Error e
      | Ok (body_ty, cs) -> Ok (TFun (arg_ty, body_ty), cs)
    end
  | App (e1, e2) ->
    begin
      match constrs_of ctxt e1 with
      | Error e -> Error e
      | Ok (t1, cs1) ->
        begin
          match constrs_of ctxt e2 with
          | Error e -> Error e
          | Ok (t2, cs2) ->
            let a = fresh () in
            Ok (a, (t1, TFun (t2, a)) :: cs1 @ cs2)
        end
    end
  | Let {is_rec = false; name = x; binding = e1; body = e2} ->
    (match constrs_of ctxt e1 with
    | Error e -> Error e
    | Ok (t1, cs1) ->
      let new_ctxt = Env.add x ([], t1) ctxt in
      (match constrs_of new_ctxt e2 with
      | Error e -> Error e
      | Ok (t2, cs2) -> Ok (t2, cs1 @ cs2)
      )
    )
  | Let {is_rec = true; name = f; binding = e1; body = e2} ->
    let a = fresh () in
    let binding_ctxt = Env.add f ([], a) ctxt in
    (match constrs_of binding_ctxt e1 with
    | Error e -> Error e
    | Ok (t1, cs1) ->
      let body_ctxt = Env.add f ([], t1) ctxt in
      (match constrs_of body_ctxt e2 with
      | Error e -> Error e
      | Ok (t2, cs2) -> Ok (t2, [(a, t1)] @ cs1 @ cs2)
      )
    )
  | Match (e1, branches) ->
    begin
      match constrs_of ctxt e1 with
      | Error e -> Error e
      | Ok (match_ty, match_cs) ->
        let a = fresh () in
        let rec process_branches branches =
          match branches with
          | [] -> Ok []
          | (p, branch_e) :: rest ->
            begin
              match constrs_of_pattern ctxt p with
              | Error e -> Error e
              | Ok (pat_ty, pat_cs, pat_ctxt) ->
                let branch_ctxt = merge_ctxts [ctxt; pat_ctxt] in
                begin
                  match constrs_of branch_ctxt branch_e with
                  | Error e -> Error e
                  | Ok (branch_ty, branch_cs) ->
                    let current_cs =
                    [(pat_ty, match_ty); (branch_ty, a)] @ pat_cs @ branch_cs
                    in
                    begin
                      match process_branches rest with
                      | Error e -> Error e
                      | Ok rest_cs -> Ok (current_cs @ rest_cs)
                    end
                end
            end
        in
        begin
          match process_branches branches with
          | Error e -> Error e
          | Ok branch_cs -> Ok (a, match_cs @ branch_cs)
        end
    end

  let type_of_expr (ctxt : ctxt) (e : expr) : (ty_scheme, Error_msg.t) result =
    match constrs_of ctxt e with
    | Error e -> Error e
    | Ok (ty, cs) ->
      begin
        match unify cs with
        | Error (t1, t2) -> Error (exp_ty e.pos t1 t2)
        | Ok sol -> Ok (principal sol ty)
    end


let well_typed (p : stmt list) : (unit, Error_msg.t) result =
  let rec go (used_ty_names : string list) (ctxt : ctxt) p =
    match p with
    | [] -> Ok ()
    | {pos; stmt=SLet {is_rec;name;binding}} :: ps ->
      let body = Ast.Expr.var dummy_pos name in
      let e = Ast.Expr.let_ pos is_rec name [] None binding body in
      begin
        match type_of_expr ctxt e with
        | Ok ty -> go used_ty_names (Env.add name ty ctxt) ps
        | Error e -> Error e
      end
    | {pos; stmt=SAdt {tpars; name; constrs}} :: ps ->
      if nub tpars = tpars
      then
        if List.mem name used_ty_names
        then Error (dup_ty_name pos name)
        else
          let rec process ctxt cs =
            match cs with
            | [] -> Ok ctxt
            | (cons_name, None) :: cs ->
              let tparams = List.map (fun x -> TParam x) tpars in
              process (Env.add cons_name (tpars, TAdt(tparams, name)) ctxt) cs
            | (cons_name, Some ty) :: cs ->
              begin
                match List.(find_opt (fun x -> not (mem x tpars)) (free_vars ty)) with
                | None ->
                  let tparams = List.map (fun x -> TParam x) tpars in
                  let ctxt = Env.add cons_name (tpars, TFun (ty, TAdt(tparams, name))) ctxt in
                  process ctxt cs
                | Some a -> Error (unbound_ty_var pos a)
              end
          in
          match process ctxt constrs with
          | Ok ctxt -> go (name :: used_ty_names) ctxt ps
          | Error e-> Error e
      else Error (ty_param_several_times pos)
  in
  let ctxt =
    Env.(
      empty
      |> add "print_endline" ([], TFun (TString, TUnit))
      |> add "Nil" (["a"], TAdt ([TParam "a"], "list"))
      |> add "Cons" (["a"], TFun (TTuple [TParam "a"; TAdt ([TParam "a"], "list")], TAdt ([TParam "a"], "list")))
    )
  in go [] ctxt p

(* EVALUATION
   ----------------------------------------------------------------------
*)

type value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VString of string
  | VCons of string * value option
  | VTuple of value list
  | VClos of {
      env : value Env.t;
      name : string option;
      arg : string;
      body : expr;
    }

type dyn_env = value Env.t

exception Div_by_zero of pos
exception Assert_fail of pos
exception Match_fail of pos
exception Compare_fun_val of pos



let subst_constr t a (t1, t2) =
    subst t a t1, subst t a t2
  
let env_union (env1 : dyn_env option) (env2 : dyn_env option) : dyn_env option =
  match env1, env2 with
  | Some env1, Some env2 -> Some (Env.fold (fun x v acc -> Env.add x v acc) env2 env1)
  | _ -> None

let rec match_pattern (v : value) (p : pattern) : dyn_env option =
  match v, p.pattern with
  | _, PWild -> Some Env.empty
  | v, PVar x -> Some (Env.add x v Env.empty)
  | VUnit, PUnit -> Some Env.empty
  | VBool b1, PBool b2 when b1 = b2 -> Some Env.empty
  | VInt n1, PInt n2 when n1 = n2 -> Some Env.empty
  | VString s1, PString s2 when s1 = s2 -> Some Env.empty
  | VTuple v_lst, PTuple p_lst ->
    if List.length v_lst = List.length p_lst
    then
      List.fold_left2
        (fun acc v p -> env_union acc (match_pattern v p))
        (Some Env.empty)
        v_lst
        p_lst
    else 
      None
  | VCons (c1, None), PCons (c2, None) when c1 = c2 -> Some Env.empty
  | VCons (c1, Some v), PCons (c2, Some p) when c1 = c2 -> match_pattern v p
  | _ -> None

let rec compare pos v : unit =
  match v with
  | VClos _ -> raise (Compare_fun_val pos)
  | VTuple v_lst ->
    List.fold_left
      (fun _ v1 -> compare pos v1) () v_lst
  | VCons (_, Some v1) -> compare pos v1
  | _ -> ()
  
let rec eval_expr (env : dyn_env) (e : Ast.Expr.t) : value =
  match e.expr with
  | Bool x -> VBool x
  | Unit -> VUnit
  | Int x -> VInt x
  | String x -> VString x
  | Negate e1 ->
    (match eval_expr env e1 with
    | VInt v -> VInt (-v)
    | _ -> assert false)
  | Bop (bop, e1, e2) ->
    (match bop with
    | Add ->
      (match eval_expr env e1, eval_expr env e2 with
      | VInt v1, VInt v2 -> VInt (v1 + v2)
      | _ -> assert false)
    | Sub ->
      (match eval_expr env e1, eval_expr env e2 with
      | VInt v1, VInt v2 -> VInt (v1 - v2)
      | _ -> assert false)
    | Mul ->
      (match eval_expr env e1, eval_expr env e2 with
      | VInt v1, VInt v2 -> VInt (v1 * v2)
      | _ -> assert false)
    | Div ->
      (match eval_expr env e1, eval_expr env e2 with
      | VInt v1, VInt v2 ->
        if v2 = 0
        then raise (Div_by_zero e.pos)
        else VInt (v1 / v2)
      | _ -> assert false)
    | Mod ->
      (match eval_expr env e1, eval_expr env e2 with
      | VInt v1, VInt v2 ->
        if v2 = 0
        then raise (Div_by_zero e.pos)
        else VInt (v1 mod v2)
      | _ -> assert false)
    | Eq ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      compare e.pos v1;
      compare e.pos v2;
      VBool (v1 = v2)
    | Neq ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      compare e.pos v1;
      compare e.pos v2;
      VBool (v1 <> v2)
    | Lt ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      compare e.pos v1;
      compare e.pos v2;
      VBool (v1 < v2)
    | Lte ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      compare e.pos v1;
      compare e.pos v2;
      VBool (v1 <= v2)
    | Gt ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      compare e.pos v1;
      compare e.pos v2;
      VBool (v1 > v2)
    | Gte ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      compare e.pos v1;
      compare e.pos v2;
      VBool (v1 >= v2)
    | And ->
      (match eval_expr env e1 with
      | VBool true -> eval_expr env e2
      | VBool false -> VBool false
      | _ -> assert false)
    | Or ->
      (match eval_expr env e1 with
      | VBool true -> VBool true
      | VBool false -> eval_expr env e2
      | _ -> assert false)
    | Concat ->
      (match eval_expr env e1, eval_expr env e2 with
      | VString v1, VString v2 -> VString (v1 ^ v2)
      | _ -> assert false))
  | If (e1, e2, e3) ->
    (match eval_expr env e1 with
    | VBool true -> eval_expr env e2
    | VBool false -> eval_expr env e3
    | _ -> assert false)
  | Annot (e1, _) -> eval_expr env e1
  | Tuple e_lst ->
    let v_tuple = List.map (fun e -> eval_expr env e) e_lst in
    VTuple v_tuple
  | Assert e1 ->
    (match eval_expr env e1 with
    | VBool true -> VUnit
    | VBool false -> raise (Assert_fail e.pos)
    | _ -> assert false)
  | Var x -> Env.find x env
  | Cons (x, e) ->
    (match e with
    | None -> VCons (x, None)
    | Some e1 ->
      let v = eval_expr env e1 in
      VCons (x, Some v))
  | Fun ((x, _), e1) -> VClos
    {
      env = env;
      name = None;
      arg = x;
      body = e1;
    }
  | App (e1, e2) ->
    let func_clos = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let e, env2 =
      (match func_clos with
      | VClos {env = env; name = None; arg = x; body = e} -> e, Env.add x v2 env
      | VClos {env = env; name = Some f; arg = x; body = e} ->
        let new_env = Env.add f func_clos env in
        e, Env.add x v2 new_env
      | _ -> assert false)
    in
    eval_expr env2 e
  | Let {is_rec = false; name = x; binding = e1; body = e2} ->
    let v1 = eval_expr env e1 in
    let env2 = Env.add x v1 env in
    eval_expr env2 e2
  | Let {is_rec = true; name = f; binding = e1; body = e2} ->
    (match eval_expr env e1 with
    | VClos {env = env; name = None; arg = x; body = body_expr} ->
      let function_clos =
        VClos
          {
            env = env;
            name = Some f;
            arg = x;
            body = body_expr;
          }
      in
      let env2 = Env.add f function_clos env in
      eval_expr env2 e2
    | _ -> assert false)
  | Match (e0, patterns) ->
    let v0 = eval_expr env e0 in
    let matched_pattern =
      List.find_map
        (fun (p, expr) ->
          match match_pattern v0 p with
          | Some e' -> Some (e', expr)
          | None -> None)
        patterns
    in
    (match matched_pattern with
    | Some (e', expr) ->
      (match env_union (Some env) (Some e') with
      | Some e'' -> eval_expr e'' expr
      | None -> assert false)
    | None -> raise (Match_fail e.pos))
  

let eval (p : stmt list) : value =
  let rec go env v p =
    match p with
    | [] -> Option.value ~default:VUnit v
    | {pos; stmt=SLet {is_rec; name; binding}} :: ps ->
      let body = {pos=dummy_pos; expr=Var name} in
      let e = Ast.Expr.let_ pos is_rec name [] None binding body in
      let v = eval_expr env e in
      go (Env.add name v env) (Some v) ps
    | _ :: ps -> go env v ps
  in
  let env =
    Env.(
      empty
      |> add "print_endline"
        (VClos
           {
             env = empty;
             name = None;
             arg = "$print_endline";
             body = Ast.Expr.mk dummy_pos Unit;
           })
    )
  in go env None p


(* INTERPRETER
   ----------------------------------------------------------------------
*)

let interp ~(filename : string) : (value, Error_msg.t) result =
  let ( let* ) = Result.bind in
  let* prog = Syntax.parse ~filename in
  let* () = well_typed prog in
  let* v =
    match eval prog with
    | v -> Ok v
    | exception Assert_fail pos -> Error (Error_msg.mk pos "(Exception) Assert_fail")
    | exception Div_by_zero pos -> Error (Error_msg.mk pos "(Exception) Div_by_zero")
    | exception Match_fail pos -> Error (Error_msg.mk pos "(Exception) Match_fail")
    | exception Compare_fun_val pos -> Error (Error_msg.mk pos "(Exception) Compare_fun_val")
  in
  Ok v

(* TESTING STUFF
   ----------------------------------------------------------------------
*)

let parse_expr s =
  let s = "let _x = " ^ s in
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | [{pos=_;stmt=SLet {binding=e;_}}] -> e
  | _ -> assert false
