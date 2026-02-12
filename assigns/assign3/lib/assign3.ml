
let is_ws c =
  match c with
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let is_digit c =
  let c = int_of_char c in
  48 <= c && c <= 57

let is_upper c =
  let c = int_of_char c in
  65 <= c && c <= 90

let all_upper x =
  let rec loop i =
    if i >= String.length x
    then true
    else if not (is_upper x.[i])
    then false
    else loop (i + 1)
  in loop 0

let rec drop_last l =
  match l with
  | x:: y :: rest -> x :: drop_last (y :: rest)
  | _ -> []

let splitRightmost ops expr = 
  let rec loop l depth rightSide =
    match l with
    | [] -> []
    | ")" :: rest -> loop rest (depth + 1) (")" :: rightSide)
    | "(" :: rest -> loop rest (depth - 1) ("(" :: rightSide)
    | op :: rest when depth = 0 && (List.mem op ops) ->
        [(List.rev rest, op, rightSide)]
    | x :: rest -> loop rest depth (x :: rightSide)
  in 
  loop (List.rev expr) 0 []
  

let lex s =
  let rec go acc i =
    let rec go_digits acc i j =
      if i + j >= String.length s
      then List.rev (String.sub s i j :: acc)
      else if is_digit (String.get s (i + j))
      then go_digits acc i (j + 1)
      else go (String.sub s i j :: acc) (i + j)
    in
    let rec go_vars acc i j =
      if i + j >= String.length s
      then List.rev (String.sub s i j :: acc)
      else if is_upper (String.get s (i + j))
      then go_vars acc i (j+1)
      else go (String.sub s i j :: acc) (i+j)
    in
    if i >= String.length s
    then List.rev acc
    else
      match String.get s i with
      | '+' -> go ("+" :: acc) (i + 1)
      | '-' -> go ("-" :: acc) (i + 1)
      | '*' -> go ("*" :: acc) (i + 1)
      | '/' -> go ("/" :: acc) (i + 1)
      | '(' -> go ("(" :: acc) (i + 1)
      | ')' -> go (")" :: acc) (i + 1)
      | '=' -> go ("=" :: acc) (i + 1)
      | c ->
        if is_digit c
        then go_digits acc i 1
        else if is_upper c
        then go_vars acc i 1
        else if is_ws c
        then go acc (i + 1)
        else assert false
  in go [] 0

let rec eval (env : (string * int) list) (expr : string list) : int =
  match splitRightmost ["+"; "-"] expr with
  | [(lhs, "+", rhs)] -> (eval env lhs) + (eval_mul_div env rhs)
  | [(lhs, "-", rhs)] -> (eval env lhs) - (eval_mul_div env rhs)
  | _ -> eval_mul_div env expr

  and eval_mul_div env expr =
    match splitRightmost ["*"; "/"] expr with
    | [(lhs, "*", rhs)] -> (eval_mul_div env lhs) * (eval_num_paren env rhs)
    | [(lhs, "/", rhs)] -> (eval_mul_div env lhs) / (eval_num_paren env rhs)
    | _ -> eval_num_paren env expr
  
  and eval_num_paren env expr =
    match expr with
    | [n] -> 
      if is_digit (String.get n 0)
      then int_of_string n
      else List.assoc n env
    | "(" :: t ->
       eval env (drop_last t)
    | _ -> assert false
  

let insert_uniq (k : 'k) (v : 'v) (r : ('k * 'v) list) : ('k * 'v) list =
  let rec loop acc remaining =
    match remaining with
    | [] -> (k, v) :: acc          
    | (key, _) :: xs when key = k ->   
        loop acc xs 
    | x :: xs ->                     
        loop (x :: acc) xs
  in loop [] r

let interp (input : string) (env : (string * int) list) : int * (string * int) list =
  match lex input with
  | var :: "=" :: expr -> (
    match eval env expr with
    | ouput -> ouput, insert_uniq var ouput env
    | exception _ -> failwith "whoops!"
  )
  | _
  | exception _ -> failwith "whoops!"
