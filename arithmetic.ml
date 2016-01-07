open Core.Std

(* trial division *)
let is_prime i =
  if i <= 1 then false
  else if i = 2 then true
  else
    let rec aux n =
      if i mod n = 0 then false
      else if n*n >= i then true
      else aux (n+1)
    in
    aux 2
;;

(* euclid's algorithm *)
let gcd num1 num2 =
  let (greater, lesser) =
    if num1 >= num2 then (num1,num2) else (num2, num1)
  in
  let rec aux a b =
    let remainder = a mod b in
    if remainder = 0 then b else aux b remainder
  in
  aux greater lesser
;;

let coprime num1 num2 =
  gcd num1 num2 = 1
;;

let phi num =
  if num = 1 then 1 else
    List.range 1 num |> List.filter ~f:(coprime num) |> List.length
;;

(* trial division *)
let factors num =
  if num < 2 then []
  else
    let rec aux i remainder acc =
      if i*2 > num then acc
      else if remainder mod i = 0 then aux i (remainder / i) (i::acc)
      else aux (i+1) remainder acc
    in
    List.rev (aux 2 num [])
;;

let factors2 num =
  match factors num with
  | [] -> []
  | x::xs ->
    let rec aux rem acc cur =
      match rem, cur with
      | [], _ -> cur::acc
      | y::ys, (u,cnt) ->
        if y = u then aux ys acc (u,cnt+1)
        else aux ys (cur::acc) (y,1)
    in
    aux xs [] (x,1) |> List.rev
;;
