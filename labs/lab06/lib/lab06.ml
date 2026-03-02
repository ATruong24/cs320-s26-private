
let rec map (f : 'a -> 'b) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | h :: t ->f h :: map f t

let rec filter (f : 'a -> bool) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | x :: xs when f x -> x :: filter f xs
  | _ :: xs -> filter f xs

let rec fold_left
    (f : 'acc -> 'a -> 'acc)
    (base : 'acc)
    (l : 'a list) : 'acc =
  match l with
  | [] -> base
  | h :: t -> fold_left f (f base h) t

let rec fold_right
    (f : 'a -> 'acc -> 'acc)
    (l : 'a list)
    (base : 'acc) : 'acc =
  match l with
  | [] -> base
  | x :: xs -> f x (fold_right f xs base)

module Matrix = struct
  type 'a t = 'a list list
  let init (dim : int * int) (f: int -> int -> 'a) : 'a t =
    let (w,h) = dim in
    List.init h (fun y -> List.init w (fun x -> f x y)) 

  let init (dim : int * int) (f : int -> int -> 'a) : 'a t =
    ignore (dim, f); assert false

  let map (f : 'a -> 'b) (m : 'a t) : 'b t =
    map (fun row -> map f row) m

  let fold_left
      (f : 'acc -> 'a -> 'acc)
      (base : 'acc)
      (m : 'a t) : 'acc =
    fold_left (fun acc row -> fold_left f acc row) base m
end