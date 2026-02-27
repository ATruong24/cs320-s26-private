
let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

type token = Lpar | Rpar | Word of string

let tokens_of_string (s : string) : token list =
  let rec go acc i =
    if i >= String.length s
    then acc
    else
      match s.[i] with
      | '(' -> go (Lpar :: acc) (i + 1)
      | ')' -> go (Rpar :: acc) (i + 1)
      | c ->
        if is_ws c
        then go acc (i + 1)
        else
          let rec go' j =
            if i + j >= String.length s
            then Word (String.sub s i j) :: acc
            else
              let c = s.[i + j] in
              if List.mem c ['('; ')'] || is_ws c
              then go (Word (String.sub s i j) :: acc) (i + j)
              else go' (j + 1)
          in go' 1
  in List.rev (go [] 0)

type sexpr =
  | Atom of string
  | List of sexpr list

let sexpr_of_tokens_opt (ts : token list) : sexpr option =
  let rec go (ts : token list) : (sexpr * token list) option =
    match ts with
    | [] -> None
    | t :: ts -> (
        match t with
        | Word a -> Some (Atom a, ts)
        | Lpar -> (
            match go' ts with
            | es, Rpar :: ts -> Some (List es, ts)
            | _ -> None
          )
        | Rpar -> None
      )
  and go' (ts : token list) : sexpr list * token list =
    match go ts with
    | Some (e, ts) ->
      let (es, ts) = go' ts in
      e :: es, ts
    | None -> [], ts
  in
  match go ts with
  | Some (e, []) -> Some e
  | _ -> None

let sexpr_of_string_opt (s : string) : sexpr option =
  sexpr_of_tokens_opt (tokens_of_string s)

let rec string_of_sexpr (e : sexpr) : string =
  match e with
  | Atom s -> s
  | List ss -> "(" ^ String.concat " " (List.map string_of_sexpr ss) ^ ")"

type op = Add | Mul | Eq

let string_of_op (op : op) : string =
  match op with
  | Add -> "+"
  | Mul -> "*"
  | Eq -> "="

let op_of_sexpr_opt (s : sexpr) : op option =
  match s with
  | Atom "+" -> Some Add
  | Atom "*" -> Some Mul
  | Atom "=" -> Some Eq
  | _ -> None

type expr =
  | Int of int
  | Bop of op * expr * expr
  | If of expr * expr * expr

let rec expr_of_sexpr_opt (s : sexpr) : expr option =
  match s with
  | Atom n -> (
    match int_of_string_opt n with
      | Some i -> Some (Int i)
      | None -> None
  )
  | List [Atom "+"; e1; e2] -> (
    match expr_of_sexpr_opt e1, expr_of_sexpr_opt e2 with
      | Some expr1, Some expr2 -> Some (Bop (Add, expr1, expr2))
      | _ -> None
  )
  | List [Atom "*"; e1; e2] -> (
    match expr_of_sexpr_opt e1, expr_of_sexpr_opt e2 with
    | Some expr1, Some expr2 -> Some (Bop (Mul, expr1, expr2))
    | _ -> None
  )
| List [Atom "="; e1; e2] -> (
    match expr_of_sexpr_opt e1, expr_of_sexpr_opt e2 with
    | Some expr1, Some expr2 -> Some (Bop (Eq, expr1, expr2))
    | _ -> None
  )
  | List [Atom "if"; e1; e2; e3] -> (
    match expr_of_sexpr_opt e1, expr_of_sexpr_opt e2, expr_of_sexpr_opt e3 with
    | Some expr1, Some expr2, Some expr3 -> Some (If (expr1, expr2, expr3))
    | _ -> None
  )
| _ -> None

let expr_of_string_opt (s : string) : expr option =
  match sexpr_of_string_opt s with
  | Some sexpr -> expr_of_sexpr_opt sexpr
  | None -> None

let rec sexpr_of_expr (e : expr) : sexpr =
  match e with
  | Int n -> Atom (string_of_int n)
  | Bop (op, e1, e2) -> List[Atom(string_of_op op); sexpr_of_expr e1; sexpr_of_expr e2]
  | If (e1, e2, e3) -> List[Atom "if"; sexpr_of_expr e1; sexpr_of_expr e2; sexpr_of_expr e3]

let string_of_expr (e : expr) : string =
  string_of_sexpr (sexpr_of_expr e)

type ty = BoolT | IntT

let ty_of_sexpr_opt (e : sexpr) : ty option =
  match e with
  | Atom s -> (
      match s with
      | "bool" -> Some BoolT
      | "int" -> Some IntT
      | _ -> None
    )
  | _ -> None

let string_of_ty (ty : ty) : string =
  match ty with
  | BoolT -> "bool"
  | IntT -> "int"

type ty_jmt =
  {
    expr : expr;
    ty : ty;
  }

let string_of_ty_jmt (j : ty_jmt) : string =
  string_of_expr j.expr ^ " : " ^ string_of_ty j.ty

type ty_rule =
  | Int_lit
  | Add_int
  | Mul_int
  | Eq_rule
  | If_rule

let string_of_ty_rule (r : ty_rule) =
  match r with
  | Int_lit -> "intLit"
  | Add_int -> "addInt"
  | Mul_int -> "mulInt"
  | Eq_rule -> "eq"
  | If_rule -> "if"

let ty_rule_of_sexpr_opt (e : sexpr) : ty_rule option =
  match e with
  | Atom s -> (
      match s with
      | "INTLIT" -> Some Int_lit
      | "ADDINT" -> Some Add_int
      | "MULINT" -> Some Mul_int
      | "EQ" -> Some Eq_rule
      | "IF" -> Some If_rule
      | _ -> None
    )
  | _ -> None

type ty_deriv =
  | Rule_app of {
      prem_derivs : ty_deriv list;
      concl : ty_jmt;
      rname : ty_rule;
    }
  | Hole

let rec ty_deriv_of_sexpr_opt (s : sexpr) : ty_deriv option =
  let rec get_prems = function
  | [] -> Some[]
  | h :: t -> (
    match ty_deriv_of_sexpr_opt h, get_prems t with
    | Some d, Some ds -> Some (d :: ds)
    | _ -> None
  )
  in
  match s with
  | Atom "???" -> Some Hole
  | List (e_s :: ty_s :: rn_s :: prems_s) -> (
    match expr_of_sexpr_opt e_s, ty_of_sexpr_opt ty_s, ty_rule_of_sexpr_opt rn_s, get_prems prems_s with
    | Some expr, Some ty, Some rname, Some prem_derivs ->
      Some (Rule_app {prem_derivs; concl = {expr; ty}; rname})
    | _ -> None
  )
| _ -> None

let ty_deriv_of_string_opt (s : string) : ty_deriv option =
  match sexpr_of_string_opt s with
  | Some sexpr -> ty_deriv_of_sexpr_opt sexpr
  | None -> None

let string_of_ty_deriv (d : ty_deriv) : string =
  let rec go d =
    match d with
    | Hole -> [("???", "hole")]
    | Rule_app d ->
      (string_of_ty_jmt d.concl, string_of_ty_rule d.rname) :: go' [] d.prem_derivs
  and go' has_line ds =
    let lines =
      List.fold_left
        (fun acc b -> (if b then "│  " else "   ") ^ acc)
        ""
        has_line
    in
    match ds with
    | [] -> []
    | [Hole] ->
      [lines ^ "└──???" , "hole"]
    | [Rule_app d] ->
      let next_line =
        ( lines ^ "└──" ^ string_of_ty_jmt d.concl
        , string_of_ty_rule d.rname
        )
      in next_line :: go' (false :: has_line) d.prem_derivs
    | Hole :: ds -> (lines ^ "├──???" , "hole") :: go' has_line ds
    | Rule_app d :: ds ->
      let next_line =
        ( lines ^ "├──" ^ string_of_ty_jmt d.concl
        , string_of_ty_rule d.rname
        )
      in
      next_line
      :: go' (true :: has_line) d.prem_derivs
      @ go' has_line ds
  in
  let lines = go d in
  let length = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun x _ -> x + 1) 0 in
  let width =
    List.fold_left
      (fun acc (line, _) -> max acc (length line))
      0
      lines
  in
  let lines =
    List.map
      (fun (line, rname) ->
         String.concat ""
           [
             line;
             String.init (width - length line + 2) (fun _ -> ' ');
             "("; rname; ")";
           ])
      lines
  in
  String.concat "\n" lines

let check_rule (r : ty_rule) (prems : ty_jmt option list) (concl : ty_jmt) : bool =
  let works p expected_e expected_t =
    match p with
    | None -> true
    | Some j -> j.expr = expected_e && j.ty = expected_t
  in

  match r, prems, concl.expr, concl.ty with
  | Int_lit, [], Int _, IntT -> true
  | Add_int, [p1; p2], Bop (Add, e1, e2), IntT -> 
    works p1 e1 IntT && works p2 e2 IntT
  | Mul_int, [p1; p2], Bop (Mul, e1, e2), IntT -> 
    works p1 e1 IntT && works p2 e2 IntT
  | Eq_rule, [p1; p2], Bop (Eq, e1, e2), BoolT -> 
    (works p1 e1 IntT && works p2 e2 IntT) ||
    (works p1 e1 BoolT && works p2 e2 BoolT)
  | If_rule, [p1; p2; p3], If (e1, e2, e3), t -> 
    works p1 e1 BoolT && works p2 e2 t && works p3 e3 t
  | _ -> false

type status =
  | Complete
  | Invalid
  | Partial

let rec check_deriv (d : ty_deriv) : status =
  match d with
  | Hole -> Partial
  | Rule_app { prem_derivs; concl; rname } ->
      let get_concl = function
        | Hole -> None
        | Rule_app r -> Some r.concl
      in
      let prem_jmts = List.map get_concl prem_derivs in
      
      if not (check_rule rname prem_jmts concl) then Invalid
      else
        let statuses = List.map check_deriv prem_derivs in
        if List.mem Invalid statuses then Invalid
        else if List.mem Partial statuses then Partial
        else Complete

type value = BoolV of bool | IntV of int

let string_of_value (v : value) : string =
  match v with
  | BoolV b -> string_of_bool b
  | IntV n -> string_of_int n

let rec value_of_expr (e : expr) : value =
  match e with
  | Int n -> IntV n
  | Bop (Add, e1, e2) -> (
      match value_of_expr e1, value_of_expr e2 with
      | IntV v1, IntV v2 -> IntV (v1 + v2)
      | _ -> failwith "Not Possible"
    )
  | Bop (Mul, e1, e2) -> (
      match value_of_expr e1, value_of_expr e2 with
      | IntV v1, IntV v2 -> IntV (v1 * v2)
      | _ -> failwith "Not Possible"
    )
  | Bop (Eq, e1, e2) -> (
    match value_of_expr e1, value_of_expr e2 with
    | IntV v1, IntV v2 -> BoolV (v1 = v2)
    | BoolV v1, BoolV v2 -> BoolV (v1 = v2)
    | _ -> failwith "Not Possible"
  )
| If (e1, e2, e3) -> (
    match value_of_expr e1 with
    | BoolV true -> value_of_expr e2
    | BoolV false -> value_of_expr e3
    | _ -> failwith "Not Possible"
  )

type error = Parse_error | Invalid_deriv of ty_deriv

let interp (s : string) : (ty_deriv * value option, error) result  =
  match ty_deriv_of_string_opt s with
  | Some deriv -> (
    match check_deriv deriv with
    | Complete -> (
      match deriv with
      | Rule_app d -> Ok (deriv, Some (value_of_expr d.concl.expr))
      | _ -> assert false
    )
    | Partial -> Ok (deriv, None)
    | Invalid -> Error (Invalid_deriv deriv)
  )
  | None -> Error Parse_error

let example_deriv : string = 
"
((if (= (= 5 (+ 1 4)) (= 0 1)) (+ 2 3) (* (+ 4 5) 67)) int IF
  ((= (= 5 (+ 1 4)) (= 0 1)) bool EQ
    ((= 5 (+ 1 4)) bool EQ
      (5 int INTLIT)
      ((+ 1 4) int ADDINT
        (1 int INTLIT)
        (4 int INTLIT)))
    ((= 0 1) bool EQ
      (0 int INTLIT)
      (1 int INTLIT)))
  ((+ 2 3) int ADDINT
    (2 int INTLIT)
    (3 int INTLIT))
  ((* (+ 4 5) 67) int MULINT
    ((+ 4 5) int ADDINT
      (4 int INTLIT)
      (5 int INTLIT))
    (67 int INTLIT)))
"
