(* CSE 130: Programming Assignment 4
 * test.ml
 * Jay Ceballos
 * A09338030
 * jjceball@ieng6.ucsd.edu 
 *)

let foldn = 
  fun f -> fun b -> fun n ->
    let rec loop = 
      fun i -> fun c ->
        if i <= n then (loop (i+1)) ((f i) c) else c in
    (loop 0) b in

let sum = (foldn (fun x -> fun y -> x + y)) 0 in

sum 10