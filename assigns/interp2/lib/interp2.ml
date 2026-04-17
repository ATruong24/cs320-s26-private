open Utils
module Error_msg = Error_msg

(* SYNTAX
   ----------------------------------------------------------------------
*)

type ty = Ast.Interp2.ty =
    | TUnit
    | TBool
    | TInt
    | TInt_list
    | TFun of ty * ty
    | TTuple of ty list

let rec pp_ty ppf ty =
  let open Fmt in
  let pp_parens ppf ty =
    match ty with
    | TFun (_, _)
    | TTuple _
    | _ -> pp_ty ppf ty
  in
  match ty with
  | TUnit -> pf ppf "unit"
  | TBool -> pf ppf "bool"
  | TInt -> pf ppf "int"
  | TFun (t1, t2) -> pf ppf "%a -> %a" pp_parens t1 pp_ty t2
  | TTuple ts -> list ~sep:(Fmt.any " * ") pp_ty ppf ts
  | TInt_list -> pf ppf "int list"

type _pattern = Ast.Interp2._pattern =
  | PUnit
  | PBool of bool
  | PInt of int
  | PNil
  | PCons of pattern * pattern
  | PTuple of pattern list
  | PVar of string
and pattern = Ast.Interp2.pattern =
  {
    pos : pos;
    pattern : _pattern;
  }

type bop = Ast.Interp2.bop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or | Cons

type _expr = Ast.Interp2._expr =
  | Unit
  | Bool of bool
  | Int of int
  | Var of string
  | Nil
  | Assert of expr
  | Negate of expr
  | Tuple of expr list
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | Fun of (string * ty) list * expr
  | App of expr * expr list
  | Let of
      {
        is_rec : bool;
        name : string;
        args : (string * ty) list;
        annot : ty option;
        binding : expr;
        body : expr;
      }
  | Match of expr * (pattern * expr) list
and expr = Ast.Interp2.expr =
  {
    pos : pos;
    expr : _expr;
  }

type _stmt = Ast.Interp2._stmt =
  | SLet of {
      is_rec : bool;
      name : string;
      args : (string * ty) list;
      annot : ty option;
      binding : expr;
    }
and stmt = Ast.Interp2.stmt =
  {
    pos : pos;
    stmt : _stmt;
  }

type prog = stmt list

module Env = Map.Make(String)

(* TYPE ERRORS
   ----------------------------------------------------------------------
*)

let unknown_var pos x = Error_msg.mk pos (Format.asprintf "Unbound value %s" x)

let exp_ty pos t1 t2 =
  let msg =
    Format.asprintf
      "This expression has type %a but an expression was expected of type %a"
      pp_ty t1 pp_ty t2
  in Error_msg.mk pos msg

let exp_pat pos t1 t2 =
  let msg =
    Format.asprintf
      "This pattern matches values of type %a but a pattern was expected which matches values of type %a"
      pp_ty t1 pp_ty t2
  in Error_msg.mk pos msg

let exp_tuple_pat pos t =
  let msg =
    Format.asprintf
      "This pattern matches values of a tuple type but a pattern was expected which matches values of type %a"
      pp_ty t
  in Error_msg.mk pos msg

let exp_diff_tuple_pat pos ty =
  let msg =
    Format.asprintf
      "This pattern matches values of a tuple type but a pattern was expected which matches values of a different tuple type %a"
      pp_ty ty
  in Error_msg.mk pos msg

let not_func pos ty =
  let msg =
    Format.asprintf
      "This expression has type %a. This is not a function; it cannot be applied"
      pp_ty ty
  in Error_msg.mk pos msg

let too_many_args pos ty =
  let msg =
    Format.asprintf
      "This function has type %a. It is applied to to many arguments"
      pp_ty ty
  in Error_msg.mk pos msg

let missing_rec_annot pos =
  Error_msg.mk pos "Must provide output type annotation for recursive function"

let missing_rec_arg pos =
  Error_msg.mk pos "Must provide argument for recursive function"

let bound_several_times pos x =
  let msg =
    Format.asprintf
      "Variable %s is bound several times in this matching"
      x
  in Error_msg.mk pos msg


(* TYPING
   ----------------------------------------------------------------------
*)

(* Contexts *)

type ctxt = ty Env.t

(* Type Checking *)

let rec type_of_expr (ctxt : ctxt) (e : expr) : (ty, Error_msg.t) result =
  match e.expr with
  | Unit -> Ok TUnit
  | Bool _ -> Ok TBool
  | Int _ -> Ok TInt

  | Var x -> 
    (match Env.find_opt x ctxt with
    | Some ty -> Ok ty
    | None -> assert false)

  | Negate e1 ->
    (match type_of_expr ctxt e1 with
    | Ok TInt -> Ok TInt
    | _ -> assert false)

  | If (e1, e2, e3) ->
    (match type_of_expr ctxt e1, type_of_expr ctxt e2, type_of_expr ctxt e3 with
    | Ok TBool, Ok t2, Ok t3 when t2 = t3 -> Ok t2
    | _ -> assert false)

  | Assert e1 ->
    (match type_of_expr ctxt e1 with
    | Ok TBool -> Ok TUnit
    | _ -> assert false)

  | Bop (op, e1, e2) ->
    (match op, type_of_expr ctxt e1, type_of_expr ctxt e2 with
    | (Add | Sub | Mul | Div | Mod), Ok TInt, Ok TInt -> Ok TInt
    | (Eq | Neq | Lt | Lte | Gt | Gte), Ok t1, Ok t2 when t1 = t2 -> Ok TBool
    | (And | Or), Ok TBool, Ok TBool -> Ok TBool
    | Cons, Ok TInt, Ok TInt_list -> Ok TInt_list
    | _ -> assert false)

  | Fun (args, body) ->
    let rec check_fun remaining_args ctxt =
      match remaining_args with
      | [] ->
        type_of_expr ctxt body
      | (x, t1) :: rest_args ->
        let new_ctxt = Env.add x t1 ctxt in
        (match check_fun rest_args new_ctxt with
        | Ok t2 -> Ok (TFun (t1, t2))
        | Error err -> Error err)
    in 
    check_fun args ctxt

  | App (e1, e2s) ->
    (match type_of_expr ctxt e1 with
    | Ok ty ->
      let rec check_args ty remaining_args =
        match ty, remaining_args with
        | TFun (t1, t2), e2 :: rest_args ->
          (match type_of_expr ctxt e2 with
          | Ok t2_arg when t1 = t2_arg -> check_args t2 rest_args
          | Ok t2_arg -> Error (exp_ty e2.pos t2_arg t1)
          | Error err -> Error err)
        | current_ty, [] -> Ok current_ty
        | _, _ -> Error (too_many_args e1.pos ty)
      in check_args ty e2s
    | Error err -> Error err)

  | Let {is_rec; name; args; annot; binding; body; _ } ->
      let ctxr_args = List.fold_left (fun c (x, t) -> Env.add x t c) ctxt args in
      let fun_ty ret = List.fold_right (fun (_, t) acc -> TFun (t, acc)) args ret in
      let ctxr =
        if is_rec then
          match annot with
          | Some t -> Env.add name (fun_ty t) ctxr_args
          | None -> assert false
        else
          ctxr_args
      in
      (match type_of_expr ctxr binding with 
       | Ok binding_ty -> type_of_expr (Env.add name (fun_ty binding_ty) ctxt) body
       | _ -> assert false)

  | Nil -> Ok TInt_list

  | Tuple es ->
    let rec check_tuple es_list acc =
      match es_list with
      | [] -> Ok (TTuple (List.rev acc))
      | e_i :: rest ->
          (match type_of_expr ctxt e_i with
            | Ok t -> check_tuple rest (t :: acc)
            | Error err -> Error err)
    in check_tuple es []

  | Match (e1, cases) ->
    (match type_of_expr ctxt e1 with
    | Ok t1 ->
      let rec check_pat p t =
        match p.pattern with
        | PUnit -> if t = TUnit then Ok Env.empty else Error (exp_pat p.pos TUnit t)
        | PBool _ -> if t = TBool then Ok Env.empty else Error (exp_pat p.pos TBool t)
        | PInt _ -> if t = TInt then Ok Env.empty else Error (exp_pat p.pos TInt t)
        | PVar "_" -> Ok Env.empty
        | PVar x -> Ok (Env.singleton x t)
        | PNil -> if t = TInt_list then Ok Env.empty else Error (exp_pat p.pos TInt_list t)

        | PCons (p1, p2) ->
                 if t = TInt_list then
                   (match check_pat p1 TInt, check_pat p2 TInt_list with
                    | Ok env1, Ok env2 ->
                        let overlap = Env.filter (fun k _ -> Env.mem k env1) env2 in
                        if not (Env.is_empty overlap) then
                          let x, _ = Env.choose overlap in Error (bound_several_times p.pos x)
                        else Ok (Env.union (fun _ _ v2 -> Some v2) env1 env2)
                    | Error err, _ | _, Error err -> Error err)
                 else Error (exp_pat p.pos TInt_list t)

        | PTuple ps ->
            (match t with
            | TTuple ts when List.length ps = List.length ts ->
                let rec check_tuple_cases ps ts =
                  match ps, ts with
                  | [], [] -> Ok Env.empty
                  | p_i :: ps_rest, t_i :: ts_rest ->
                      (match check_pat p_i t_i with
                        | Ok env_p ->
                            (match check_tuple_cases ps_rest ts_rest with
                            | Ok env_rest ->
                                let overlap = Env.filter (fun k _ -> Env.mem k env_p) env_rest in
                                if not (Env.is_empty overlap) then
                                  let x, _ = Env.choose overlap in Error (bound_several_times p.pos x)
                                else Ok (Env.union (fun _ _ v2 -> Some v2) env_p env_rest)
                            | Error err -> Error err)
                        | Error err -> Error err)
                  | _, _ -> assert false
                in check_tuple_cases ps ts
            | _ -> Error (exp_tuple_pat p.pos t))
      in
      let rec check_branches remaining_cases expected_ty =
        match remaining_cases with
        | [] -> Ok expected_ty
        | (p, branch_e) :: rest ->
            (match check_pat p t1 with
            | Ok new_env ->
                let merged_ctxt = Env.union (fun _ _ v2 -> Some v2) ctxt new_env in
                (match type_of_expr merged_ctxt branch_e with
                  | Ok branch_ty ->
                      if expected_ty = branch_ty then check_branches rest expected_ty
                      else Error (exp_ty branch_e.pos branch_ty expected_ty)
                  | Error err -> Error err)
            | Error err -> Error err)
      in 
      (match cases with
      | [] -> assert false
      | (p, branch_e) :: rest ->
          (match check_pat p t1 with
            | Ok new_env ->
                let merged_ctxt = Env.union (fun _ _ v2 -> Some v2) ctxt new_env in
                (match type_of_expr merged_ctxt branch_e with
                | Ok branch_ty -> check_branches rest branch_ty
                | Error err -> Error err)
            | Error err -> Error err))
  | Error err -> Error err)


let type_of (p : prog) : (ty, Error_msg.t) result =
  let rec go ctxt ty p =
    match p with
    | [] -> Ok (Option.value ~default:TUnit ty)
    | {pos; stmt=SLet {is_rec; name; args; annot; binding}} :: ps -> (
      let body = {pos=dummy_pos; expr=Var name} in
      let e = {pos; expr=Let {is_rec; name; args; annot; binding; body}} in
      match type_of_expr ctxt e with
      | Ok ty ->
        let ctxt = Env.add name ty ctxt in
        go ctxt (Some ty) ps
      | Error err -> Error err
    )
  in go Env.empty None p


(* EVALUATION
   ----------------------------------------------------------------------
*)

(* Values *)

type value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VTuple of value list
  | VClos of {
      env : value Env.t;
      name : string option;
      args : string list;
      body : expr;
    }
  | VInt_list of int list

(* Dynamic Environments *)

type dyn_env = value Env.t

(* Evaluation *)

exception Div_by_zero of pos
exception Assert_fail of pos
exception Match_fail of pos

let rec eval_expr (env : dyn_env) (e : expr) : value =
  match e.expr with
  | Unit -> VUnit
  | Bool b -> VBool b
  | Int n -> VInt n
  | Var x -> Env.find x env

  | Negate e1 -> 
    (match eval_expr env e1 with
      | VInt n -> VInt (-n)
      | _ -> assert false)

  | Assert e1 ->
    (match eval_expr env e1 with
    | VBool true -> VUnit
    | VBool false -> raise (Assert_fail e.pos)
    | _ -> assert false)

  | If (e1, e2, e3) ->
    (match eval_expr env e1 with
    | VBool true -> eval_expr env e2
    | VBool false -> eval_expr env e3
    | _ -> assert false)

  | Bop (op, e1, e2) ->
    (match op with
    | And->
      (match eval_expr env e1 with
      | VBool false -> VBool false
      | VBool true -> eval_expr env e2
      | _ -> assert false)
    | Or ->
      (match eval_expr env e1 with
      | VBool true -> VBool true
      | VBool false -> eval_expr env e2
      | _ -> assert false)
    | _ ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match op, v1, v2 with
      | Add, VInt n1, VInt n2 -> VInt (n1 + n2)
      | Sub, VInt n1, VInt n2 -> VInt (n1 - n2)
      | Mul, VInt n1, VInt n2 -> VInt (n1 * n2)
      | Div, VInt n1, VInt n2 ->
        if n2 = 0 then raise (Div_by_zero e.pos) else VInt (n1 / n2)
      | Mod, VInt n1, VInt n2 ->
        if n2 = 0 then raise (Div_by_zero e.pos) else VInt (n1 mod n2)
    | Eq, _, _ -> VBool (v1 = v2)
    | Neq, _, _ -> VBool (v1 <> v2)
    | Lt, _, _ -> VBool (v1 < v2)
    | Lte, _, _ -> VBool (v1 < v2)
    | Gt, _, _ -> VBool (v1 > v2)
    | Gte, _, _  -> VBool (v1 >= v2 )
    | _ -> assert false)
    )

  | Let {is_rec; name; args; binding; body; _ } ->
    let v =
        if args <> [] then 
          VClos { env; name = (if is_rec then Some name else None); args = List.map fst args; body = binding }
        else match eval_expr env binding with
          | VClos c when is_rec -> VClos { c with name = Some name } 
          | other_val -> other_val
      in 
      eval_expr (Env.add name v env) body

  | Fun (args, body) ->
      VClos {env=env; name=None; args=List.map fst args; body}

  | App (e1, args) ->
    let rec apply current_val arg_vals =
        match current_val, arg_vals with
        | result, [] -> result
        | VClos c, v :: vs ->
            (match c.args with
             | p :: ps ->
                 let env_with_self = match c.name with Some f -> Env.add f current_val c.env | None -> c.env in
                 let final_env = Env.add p v env_with_self in
                 if ps = [] then apply (eval_expr final_env c.body) vs
                 else apply (VClos { c with env = final_env; args = ps }) vs 
             | [] -> assert false)
        | _ -> assert false
      in 
      apply (eval_expr env e1) (List.map (eval_expr env) args)

  | Nil -> VInt_list []
  | Tuple es ->
    let vs = List.map (eval_expr env) es in
    VTuple vs

  | Match (e1, cases) ->
    let v0 = eval_expr env e1 in
    let rec match_cases p v =
      match p.pattern, v with 
      | PUnit, VUnit -> Some Env.empty
      | PBool b, VBool v when b = v -> Some Env.empty
      | PInt n, VInt m when n = m -> Some Env.empty
      | PNil, VInt_list [] -> Some Env.empty
      | PVar "_", _ -> Some Env.empty
      | PVar x, matchval -> Some (Env.add x matchval Env.empty)

      | PCons (p1, p2), VInt_list (v :: vs) ->
        (match match_cases p1 (VInt v), match_cases p2 (VInt_list vs) with
        | Some env1, Some env2 -> Some (Env.union (fun _ _ v2 -> Some v2) env1 env2)
        | _ -> None)

      | PTuple ps, VTuple vs when List.length ps = List.length vs ->
        List.fold_left2 (fun acc p v ->
          match acc with
          | Some env ->
            (match match_cases p v with
            | Some env' -> Some (Env.union (fun _ _ v2 -> Some v2) env env')
            | None -> None)
          | None -> None
        ) (Some Env.empty) ps vs
      | _ -> None
    in
    let rec try_cases cases =
      match cases with
      | [] -> raise (Match_fail e.pos)
      | (p, e) :: rest_cases ->
        (match match_cases p v0 with
        | Some new_env -> eval_expr (Env.union (fun _ _ v2 -> Some v2) env new_env) e
        | None -> try_cases rest_cases)
    in try_cases cases


let eval (p : prog) : value =
  let rec go env v p =
    match p with
    | [] -> Option.value ~default:VUnit v
    | {pos; stmt=SLet {is_rec; name; args; annot; binding}} :: ps ->
      let body = {pos=dummy_pos; expr=Var name} in
      let e = {pos; expr=Let {is_rec; name; args; annot; binding; body}} in
      let v = eval_expr env e in
      go (Env.add name v env) (Some v) ps
  in go Env.empty None p


(* INTERPRETER
   ----------------------------------------------------------------------
*)

let interp ~(filename : string) : (value * ty, Error_msg.t) result =
  let ( let* ) = Result.bind in
  let* prog = Syntax.parse ~filename in
  let* prog = Ast.Interp2.prog_of_prog prog in
  let* ty = type_of prog in
  let* v =
    match eval prog with
    | v -> Ok v
    | exception Assert_fail pos -> Error (Error_msg.mk pos "(Exception) Assert_fail")
    | exception Div_by_zero pos -> Error (Error_msg.mk pos "(Exception) Div_by_zero")
    | exception Match_fail pos -> Error (Error_msg.mk pos "(Exception) Match_fail")
  in
  Ok (v, ty)


(* TESTING STUFF
   ----------------------------------------------------------------------
*)

let parse_expr s =
  let s = "let _ = " ^ s in
  let p = Parser.prog Lexer.read (Lexing.from_string s) in
  match Ast.Interp2.prog_of_prog p with
  | Ok [{pos=_;stmt=SLet {binding=e;_}}] -> e
  | _ -> assert false

let parse_ty s =
  let s = "let _ : " ^ s ^ " = assert false" in
  let p = Parser.prog Lexer.read (Lexing.from_string s) in
  match Ast.Interp2.prog_of_prog p with
  | Ok [{pos=_;stmt=SLet {annot=Some ty;_}}] -> ty
  | _ -> assert false
