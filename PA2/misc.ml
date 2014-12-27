(* CSE 130: Programming Assignment 2
 * misc.ml
 * Jay Ceballos
 * A09338030
 * jjceball@ieng6.ucsd.edu 
 *)

(* assoc : int * string * (string * int) list -> int
 * or more generally, assoc : 'a * 'b * ('b * 'a) list -> 'a
 * Takes a triple (d,k,l) where l is a list of key-value pairs [(k1,v1);(k2,v2);...] 
 * and finds the first ki that equals k. If such a ki is found, then vi is returned. 
 * Otherwise, the default value d is returned.
 *
 * e.g. # assoc (-1,"jeff",[("sorin",85);("jeff",23);("moose",44)]);;
 * - : int = 23 
 * # assoc (-1,"bob",[("sorin",85);("jeff",23);("moose",44);("margaret",99)]);;
 * - : int = -1 
 *
 *  ** your function should be tail recursive **
 *)

let rec assoc (d,k,l) = 
  match l with
  | (a,b)::tl -> if k = a then b else assoc(d,k,tl)
  | [] -> d;;

(* removeDuplicates : int list -> int list 
 * or more generally, removeDuplicates : 'a list -> 'a list
 * takes a list l and returns the list of elements of l with the duplicates, 
 * i.e. second, third, etc. occurrences, removed, and where the remaining 
 * elements appear in the same order as in l
 *
 * e.g. # removeDuplicates [1;6;2;4;12;2;13;6;9];;
 * - : int list = [1;6;2;4;12;13;9] 
 *
 *  ** your function "helper" should be tail recursive **
 * for this problem only, you may use the library function List.mem and
 * List.rev
 *)

(* fill in the code wherever it says : failwith "to be written" *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
        | h::t -> 
        let seen' = if List.mem h seen then seen else h::seen in
        let rest' = t in helper (seen',rest') 
    in List.rev (helper ([],l));;


(* wwhile : (int -> int * bool) * int -> int
 * or more generally, ('a -> 'a * bool) * 'a -> 'a
 * takes as input a pair (f,b) and calls the function f on input b to get a pair (b',c'). 
 * wwhile should continue calling f on b' to update the pair as long as c' is true. 
 * Once f returns a c' that is false, wwhile should return b'.
 *
 * e.g. # let f x = let xx = x*x*x in (xx,xx<100);; 
 * val f : int -> int * bool = fn 
 * # wwhile (f,2);; 
 * - : int = 512 
 *
 *  ** your function should be tail recursive **
 *)
let rec wwhile (f, b) =
  let (b', c') = f b in
  if not c' then b' else wwhile(f, b');;


(* fixpoint : (int -> int) * int -> int
 * or more generally, fixpoint : ('a -> 'a) * 'a -> 'a
 * repeatedly updates b with f(b) until b=f(b) and then returns b.
 *
 * e.g. # let g x = truncate (1e6 *. cos (1e-6 *. float x));; 
 * val f : int -> int = fn 
 * # fixpoint (g,0);; 
 * - : int = 739085 
 *)

(* fill in the code wherever it says : failwith "to be written" *)
let fixpoint (f,b) = wwhile ((fun x -> let xx = f x in (xx, xx != x)), b)

(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      

(************** Add Testing Code Here ***************)
