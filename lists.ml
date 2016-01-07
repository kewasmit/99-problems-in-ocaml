open Core.Std;;

let last xs =
  List.reduce xs (fun _ y -> y)
;;

let rec last_two = function
  | [] | [_] -> None
  | x::y::[] -> Some (x,y)
  | _:: (x::xs as rest) -> last_two rest
;;

let last_two' = function
  | [] | [_] -> None
  | x::y::zs -> Some (List.fold_left
                        zs
                        ~f:(fun (a,b) c -> (b,c))
                        ~init:(x,y))
;;

let at loc list =
  let rec go cnt lst =
    match lst with
    | [] -> None
    | x::xs ->
      if cnt = loc then Some x
      else go (cnt+1) xs
  in go 1 list
;;

let length list =
  let rec go cnt lst =
    match lst with
    | [] -> cnt
    | _::xs -> go (cnt + 1) xs
  in
  go 0 list
;;

let length' list =
  List.fold_left
    list
    ~f:(fun acc _ -> acc + 1)
    ~init:0
;;

let rev list =
  List.fold_left
    list
    ~f:(fun acc x -> x::acc)
    ~init:[]
;;

let is_palindrome list =
  list = rev list
;;

type 'a node =
  | One of 'a
  | Many of 'a node list
;;

let rec flatten = function
  | [] as l -> l
  | (One x)::xs -> x::(flatten xs)
  | (Many xs)::xss -> (flatten xs) @ (flatten xss)
;;

let flatten' list =
  let rec go acc rem =
    match rem with
    | [] -> acc
    | (One x)::xs -> go (x::acc) xs
    | (Many x)::xs -> go (go acc x) xs
  in
  List.rev (go [] list)
;;

let rec compress = function
  | [] as l -> l
  | [_] as l -> l
  | x::y::zs ->
    if x = y then compress (y::zs)
    else x::(compress (y::zs))
;;

let compress' list =
  let aux acc y =
    match acc with
    | [] -> [y]
    | x::xs as l ->
      if x = y then l
      else y::l
  in
  List.fold_left list ~f:aux ~init:[] |> List.rev
;;

let pack list =
  let rec go acc char_acc c rem =
    match rem with
    | [] -> char_acc::acc
    | x::xs ->
      if x = c then
        go acc (c::char_acc) c xs
      else
        go (char_acc::acc) [x] x xs
  in
  match list with
  | [] -> []
  | x::xs -> go [] [x] x xs |> List.rev
;;

type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

let encode = function
  | [] -> []
  | xs -> List.map
            (pack xs)
            ~f:(fun l ->
                match l with
                | [] -> assert false
                | x::xs -> (* this is the only possible pattern *)
                  let len = List.length l in
                  if len = 1 then One x
                  else Many (len, x))
;;

let rec decode = function
  | [] -> []
  | x::xs ->
    match x with
    | One y -> y :: decode xs
    | Many (cnt, y) ->
      if cnt = 1 then
        y :: decode xs
      else
        y :: (decode (Many (cnt-1,y) :: xs))
;;

let encode_direct list =
  let rec go acc item_acc c rem =
    match rem, item_acc with
    | [], _ -> item_acc::acc
    | x::xs, ((cnt,ch) as item) ->
      if x = c then
        go acc (cnt+1,ch) c xs
      else
        go (item::acc) (1,x) x xs
  in
  let aux = function
    | (1,x) -> One x
    | (cnt,x) -> Many (cnt,x)
  in
  match list with
  | [] -> []
  | x::xs -> go [] (1,x) x xs |> List.map ~f:aux |> List.rev
;;

let duplicate xs =
  List.fold_left
    xs
    ~f:(fun acc x -> x::x::acc)
    ~init:[]
  |> List.rev
;;

let rec replicate l n =
  let rec aux cnt =
    match l with
    | [] -> []
    | x::xs ->
      if cnt < n then x :: (aux (cnt+1))
      else replicate xs n
  in
  aux 0
;;

let drop list n =
  let rec aux l cnt =
    match l with
    | [] -> []
    | x::xs ->
      if cnt <= 1 then aux xs n
      else x :: (aux xs (cnt-1))
  in
  aux list n
;;

let rec split list n =
  let rec aux rem first cnt =
    match rem with
    | [] -> (List.rev first, [])
    | x::xs as l ->
      if cnt > n then (List.rev first, l)
      else aux xs (x::first) (cnt+1)
  in
  aux list [] 1
;;

(* if the start is less than zero or the finish is greater than the
list index, this will just start at the beginning or end at the end
rather than raising an exception *)
let slice list i k =
  let rec aux rem acc cnt =
    match rem with
    | [] -> List.rev acc
    | x::xs ->
      if cnt = k then List.rev (x::acc)
      else if cnt >= i then aux xs (x::acc) (cnt+1)
      else aux xs acc (cnt+1)
  in
  aux list [] 0
;;

let rotate list magnitude =
  let len = List.length list in
  let mag = magnitude mod len in
  let target = if mag >= 0 then mag else len + mag in
  let rec aux rem acc cnt =
    match rem with
    | [] -> acc
    | x::xs ->
      if cnt = target then rem @ List.rev acc
      else aux xs (x::acc) (cnt+1)
  in
  aux list [] 0
;;

let remove_at k list =
  let rec aux rem cnt =
    match rem with
    | [] -> []
    | x::xs ->
      if cnt = k then xs
      else x :: (aux xs (cnt+1))
  in aux list 0
;;

let insert_at e k list =
  let rec aux rem cnt =
    match rem with
    | [] -> [e]
    | x::xs as l ->
      if cnt = k then e::l
      else x :: (aux xs (cnt+1))
  in aux list 0
;;

let range i j =
  let f = if j > i then (fun x -> x + 1) else (fun x -> x - 1) in
  let rec aux cur acc =
    if cur = j then List.rev (cur::acc)
    else
      let next = f cur in
      aux next (cur::acc)
  in
  aux i []
;;


(* fails when given an empty list *)
let rand_select list number =
  let arr = Array.of_list list in
  let len = Array.length arr in
  let rec aux cnt acc =
    if cnt = number then acc
    else
      let new_val = arr.(Core_kernel.Core_random.int len) in
      aux (cnt+1) (new_val::acc)
  in
  aux 0 []
;;

let lotto_select number max =
  let rec aux cnt acc =
    if cnt = number then acc
    else aux (cnt+1) (1 + (Core_kernel.Core_random.int max)::acc)
  in
  aux 0 []
;;

(* this one feels a little like cheating... *)
let permutation = function
  | [] | [_] as l -> l
  | list ->
    let arr = Array.of_list list in
    let start = Array.length arr - 1 in
    for i=start downto 1 do
      let j = Random.int (i+1) in
      let tmp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- tmp;
    done;
    Array.to_list arr
;;
