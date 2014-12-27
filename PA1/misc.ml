(* CSE 130: Programming Assignment 1
 * misc.ml
 * Jay Ceballos
 * A09338030
 * jjceball@ieng6.ucsd.edu 
 *)

(* sumList : int list -> int 
 * (sumList n) returns the sum of the elements in the list l
 * Example Call: (sumList [1; 2; 3; 4])
 * int = 10
*) 

let rec sumList l = 
	match l with
	| [] -> 0
	| (hd::tl) -> hd + sumList tl;;


(* digitsOfInt : int -> int list 
 * (digitsOfInt n) returns an empty list [] if n is less 
 * than zero, otherwise it returns the list of digits of n in order
 *)

let rec digitsOfInt n = 
	if n < 0 then []
	else if n < 10 then [n]
	else digitsOfInt(n/10) @ [(n mod 10)];;


(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)

(* additivePersistence : int -> int
 * A method that is meant to equal the number of additions required to 
 * obtain a single digit from a number n
 *)

let rec additivePersistence n =
	if n < 10 then 0
	else additivePersistence(sumList(digits(n))) + 1;;	

(* digitalRoot : int -> int 
 * A method that is meant to calculate the digit obtained from the 
 * additivePersistence method. 
 *)

let rec digitalRoot n =
	if n < 10 then n
	else digitalRoot(sumList(digits(n)));;

(* listReverse : 'a list -> 'a list
 * listReverse returns the list of elements of xs in
 * reversed order of appearance
 * Example: 
 * listReverse [1; 2; 3; 4];; is int list = [4; 3; 2; 1]
 *)
let rec listReverse l = 
	match l with 
	| [] -> []
	| (l::l') -> listReverse l' @ [l];;

(* explode : string -> char list 
 * (explode s) returns the list of characters in the string s based
 * on order of appearance
 * Example:
 * (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* palindrome : string -> bool 
 * palindrom is a bool that takes a string w and returns true if 
 * the string is a palindrome and false otherwise
 * Example: 
 * (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let palindrome w =
	if explode(w) = listReverse(explode(w)) then true
	else false;;

