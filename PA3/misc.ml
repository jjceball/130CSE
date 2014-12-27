(* CSE 130: Programming Assignment 2
 * misc.ml
 * Jay Ceballos
 * A09338030
 * jjceball@ieng6.ucsd.edu 
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)

(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)

(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

(* sqsum : int list -> int 
 * uses List.fold_left 
 *
 * that takes a list of integers [x1;...;xn]) and returns the 
 * integer: x1^2 + ... + xn^2 .
 *
 * e.g. # sqsum [];;
 * - : int = 0 
 * # sqsum [1;2;3;4] ;;
 * - : int = 30 
 * # sqsum [-1;-2;-3;-4] ;;
 * - : int = 30 
 *)

let sqsum xs = 
  let f a x = a + (x * x) in
  let base = 0 in
    List.fold_left f base xs

(* pipe : ('a -> 'a) list -> ('a -> 'a)
 * uses List.fold_left 
 * 
 * takes a list of functions [f1;...;fn]) and returns a 
 * function f such that for any x, the application f x 
 * returns the result fn(...(f2(f1 x)))
 *
 * e.g. # pipe [] 3;;
 * - : int = 3 
 * # pipe [(fun x-> 2*x);(fun x -> x + 3)] 3 ;;
 * - : int = 9 
 * # pipe [(fun x -> x + 3);(fun x-> 2*x)] 3;;
 * - : int = 12
 *
 *)

let pipe fs = 
  let f a x = fun z -> x (a z) in
  let base = fun b -> b in
    List.fold_left f base fs

(* sepConcat : string -> string list -> -> string
 * uses List.fold_left
 *  
 * takes as input a string sep to be used as a separator, 
 * and a list of strings [s1;...;sn]. If there are 0 strings 
 * in the list, then sepConcat should return "". If there is 
 * 1 string in the list, then sepConcat should return s1. 
 * Otherwise, sepConcat should return the concatination 
 * s1 sep s2 sep s3 ... sep sn
 *
 * e.g. # sepConcat ", " ["foo";"bar";"baz"];;
 * - : string = "foo, bar, baz"
 * # sepConcat "---" [];;
 * - : string = ""
 * # sepConcat "" ["a";"b";"c";"d";"e"];;
 * - : string = "abcde"
 * # sepConcat "X" ["hello"];;
 * - : string = "hello"
 *
 *)

let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a ^ sep ^ x in
      let base = h in
      let l = t in
        List.fold_left f base l

(* stringOfList : ('a -> string) -> 'a list -> string
 * The first input is a function f: 'a -> string which will be 
 * called by stringOfList to convert each element of the list 
 * to a string. The second input is a list l: 'a list, which 
 * we will think of as having the elemtns l1, l2, ..., ln
 *
 * return a string representation of the list l as a concatenation 
 * of the following: "[" (f l1) "; " (f l2) "; " (f l3) "; " ... "; " (f ln) "]"
 * 
 * e.g. # stringOfList string_of_int [1;2;3;4;5;6];;
 * - : string = "[1; 2; 3; 4; 5; 6]"
 * # stringOfList (fun x -> x) ["foo"];;
 * - : string = "[foo]"
 * # stringOfList (stringOfList string_of_int) [[1;2;3];[4;5];[6];[]];;
 * - : string = "[[1; 2; 3]; [4; 5]; [6]; []]"
 *
 *)

let stringOfList f l = "[" ^ sepConcat "; " (List.map f l) ^ "]"

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(* clone : 'a -> int -> 'a list
 * takes as input x and then takes as input an integer n. The result 
 * is a list of length n, where each element is x. If n is 0 or 
 * negative, clone should return the empty list.
 *
 * e.g. # clone 3 5;;
 * - : int list = [3; 3; 3; 3; 3] 
 * # clone "foo" 2;;
 * - : string list = ["foo"; "foo"]
 * # clone clone (-3);;
 * - : ('_a -> int -> '_a list) list = []
 *
 *)

let rec clone x n = 
  if n <= 0 then []
  else [x] @ (clone x (n-1))

(* padZero : int list -> int list -> int list * int list
 * takes two lists: [x1,...,xn] [y1,...,ym] and adds zeros in front to 
 * make the lists equal.
 * 
 * e.g. # padZero [9;9] [1;0;0;2];;
 * - : int list * int list = ([0;0;9;9],[1;0;0;2]) 
 * # padZero [1;0;0;2] [9;9];;
 * - : int list * int list = ([1;0;0;2],[0;0;9;9]) 
 *
 *)

let rec padZero l1 l2 = 
  if (List.length l1 < List.length l2) then padZero (0::l1) l2
  else if (List.length l1 > List.length l2) then padZero l1 (0::l2)
  else (l1, l2)

(* removeZero : int list -> int list 
 * takes a list and removes a prefix of trailing zeros.
 * 
 * e.g. # removeZero [0;0;0;1;0;0;2];;
 * - : int list = [1;0;0;2] 
 * # removeZero [9;9];;
 * - : int list = [9;9] 
 * # removeZero [0;0;0;0];;
 * - : int list = [] 
 *
 *)

let rec removeZero l = 
  match l with  
  | [] -> []
  | x::xs -> if x = 0 then removeZero xs else l

(* bigAdd : int list -> int list -> int list
 * takes two integer lists, where each integer is in the 
 * range [0..9] and returns the list corresponding to the 
 * addition of the two big integers. 
 * 
 * You have to fill in the implementation to supply the appropriate 
 * values to f, base, args .
 *
 * e.g. # bigAdd [9;9] [1;0;0;2];;
 * - : int list = [1;1;0;1] 
 * # bigAdd [9;9;9;9] [9;9;9];;
 * - : int list = [1;0;9;9;8] 
 *
 *)

let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = let (carry, res) = a in
    let (x1, x2) = x in
    ((x1+x2+carry)/10, ((x1+x2+carry) mod 10)::res) in
    let base = (0, []) in
    let args = List.combine (List.rev(0::l1)) (List.rev(0::l2)) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

(* mulByDigit : int -> int list -> int list
 * takes an integer digit and a big integer, and returns the big 
 * integer list which is the result of multiplying the big integer 
 * with the digit.
 *
 * e.g. # mulByDigit 9 [9;9;9;9];;
 * - : int list = [8;9;9;9;1] 
 *
 *)

let rec mulByDigit i l = 
  if i <= 0 then [0]
  else if  i = 1 then l
  else bigAdd (mulByDigit (i-1) l) l 

(* bigMul : int list -> int list -> int list
 * takes two integer lists, where each integer is in the 
 * range [0..9] and returns the list corresponding to the 
 * multiplication of the two big integers. 
 * 
 * You have to fill in the implementation to supply the appropriate 
 * values to f, base, args .
 *
 * e.g. # bigMul [9;9;9;9] [9;9;9;9];;
 * - : int list = [9;9;9;8;0;0;0;1] 
 * # bigMul [9;9;9;9;9] [9;9;9;9;9];;
 * - : int list = [9;9;9;9;8;0;0;0;0;1] 
 *
 *)

let bigMul l1 l2 = 
  let f a x = let (count, res) = a in
        let (l1num, list2) = x in
      ((count-1), (bigAdd ((mulByDigit l1num list2)@(clone 0 count)) res)) in
  let base = (((List.length l1)-1), [0]) in
  let args = List.combine l1 (clone l2 (List.length l1)) in
  let (_, res) = List.fold_left f base args in
    res    
