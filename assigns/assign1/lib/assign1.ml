
let sqrt (n : int) : int = (* CHANGE _n to n! *)
  let rec loop k =
    if k * k >= n then k 
    else loop (k+1)
  in 
    loop 0

let pow (n : int) (k : int) : int = (* CHANGE _n to n and _k to k! *)
  let rec loop base exp = 
    if exp = 0 then 1
    else base * loop base (exp - 1)
  in
  loop n k

let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let string_of_char (c : char) : string =
  String.init 1 (fun _ -> c)

let explode (s : string) : char list =
  let rec loop acc i =
    if i = String.length s
    then List.rev acc
    else loop (String.get s i :: acc) (i + 1)
  in loop [] 0

let implode (cs : char list) : string =
  String.init
    (List.length cs)
    (fun i -> List.nth cs i)

let implode_all (css : char list list) : string list =
  let rec loop acc css =
    match css with
    | [] -> List.rev acc
    | cs :: rest -> loop (implode cs :: acc) rest
  in loop [] css

let split_on_ws_helper (cs : char list) : char list list =
  let rec loop acc current_token chars =
    match chars with
    | [] -> 
        if current_token = [] then List.rev acc
        else List.rev (List.rev current_token :: acc)
    | c :: rest ->
        if is_ws c then
          if current_token = [] then 
            loop acc [] rest
          else 
            loop (List.rev current_token :: acc) [] rest
        else
          loop acc (c :: current_token) rest
  in
  loop [] [] cs

let split_on_ws (s : string) : string list =
  implode_all (split_on_ws_helper (explode s))

let rec eval (stack : int list) (prog : string list) : int list =
  match prog with
  | [] -> stack 
  | token :: rest ->
      match token with
      | "+" -> 
          (match stack with 
           | y :: x :: s -> eval ((x + y) :: s) rest
           | _ -> failwith "Stack underflow on +")
      | "-" -> 
          (match stack with 
           | y :: x :: s -> eval ((x - y) :: s) rest
           | _ -> failwith "Stack underflow on -")
      | "*" -> 
          (match stack with 
           | y :: x :: s -> eval ((x * y) :: s) rest
           | _ -> failwith "Stack underflow on *")
      | "/" -> 
          (match stack with 
           | y :: x :: s -> eval ((x / y) :: s) rest
           | _ -> failwith "Stack underflow on /")
      | "mod" -> 
          (match stack with 
           | y :: x :: s -> eval ((x mod y) :: s) rest
           | _ -> failwith "Stack underflow on mod")
      | "^" -> 
          (match stack with 
           | y :: x :: s -> eval ((pow x y) :: s) rest
           | _ -> failwith "Stack underflow on ^")
      | "sqrt" -> 
          (match stack with 
           | x :: s -> eval ((sqrt x) :: s) rest
           | _ -> failwith "Stack underflow on sqrt")
      | _ -> 
          eval (int_of_string token :: stack) rest

let interp (input : string) : int =
  match eval [] (split_on_ws input) with
  | [output] -> output
  | _ -> failwith "whoops!"
