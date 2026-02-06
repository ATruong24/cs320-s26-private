
let is_ws c =
  match c with
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let is_digit c =
  let c = int_of_char c in
  48 <= c && c <= 57

let lex s =
  let rec go acc i =
    let rec go_digits acc i j =
      if i + j >= String.length s
      then List.rev (String.sub s i j :: acc)
      else if is_digit (String.get s (i + j))
      then go_digits acc i (j + 1)
      else go (String.sub s i j :: acc) (i + j)
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
      | c ->
        if is_digit c
        then go_digits acc i 1
        else if is_ws c
        then go acc (i + 1)
        else assert false
  in go [] 0

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

let rec eval expr =
  match splitRightmost ["+"; "-"] expr with
  | [(lhs, "+", rhs)] -> (eval lhs) + (eval_mul_div rhs)
  | [(lhs, "-", rhs)] -> (eval lhs) - (eval_mul_div rhs)
  | _ -> eval_mul_div expr

and eval_mul_div expr =
  match splitRightmost ["*"; "/"] expr with
  | [(lhs, "*", rhs)] -> (eval_mul_div lhs) * (eval_num_paren rhs)
  | [(lhs, "/", rhs)] -> (eval_mul_div lhs) / (eval_num_paren rhs)
  | _ -> eval_num_paren expr

and eval_num_paren expr =
  match expr with
  | [n] -> int_of_string n
  | "(" :: rest -> eval (drop_last rest)
  | _ -> assert false

let interp (input : string) : int =
  match eval (lex input) with
  | output -> output
  | exception _ -> failwith "whoops!"
