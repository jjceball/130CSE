(* art.ml
 * cs334
 * based on code by Chris Stone
 *)

(* CSE 130: Programming Assignment 2
 * Jay Ceballos
 * A09338030
 * jjceball@ieng6.ucsd.edu 
 *)


#use "misc.ml"
#use "expr.ml"

(******************* Functions you need to write **********)

(* build: (int*int->int) * int -> Expr 
 * The function is called with the argument pair (rand,depth) .
 *
 * The first element rand is a random number generator of type int * int -> int . 
 * Each call rand (i,j) returns a random integer between i inclusive and j exclusive. 
 * Use this function to randomly select operators when composing subexpressions 
 * to build up larger expressions.
 *
 * The second element depth is a maximum nesting depth. 
 * A random expression of depth d built by randomly composing sub-expressions of 
 * depth d-1 , and the only expressions of depth 0 are VarX or VarY . 
 *)

let rec build (rand,depth) = 
  let pumping = 1+rand(0,7) in
  let alternate = rand(0, pumping) in
  if depth == 0 then (
    if alternate mod 2 == 0 then
      buildX() 
    else
      buildY()
  )
  else begin
    let x = rand(0,9) in
      let result = match x with
      |0 -> build(rand, (depth-1))
      |1 -> build(rand, (depth-1))
      |2 -> buildSine(build(rand, (depth-1))) 
      |3 -> buildCosine(build(rand, (depth - 1)))
      |4 -> buildAverage(build(rand, (depth-1)),build(rand, (depth-1)))
      |5 -> buildTimes(build(rand, (depth-1)),build(rand, (depth-1)))
      |_ -> buildThresh(build(rand, (depth-1)),build(rand, (depth-1)),build(rand, (depth-1)),build(rand, (depth-1)))
    in result
  end

(* build2: (int*int->int) * int -> Expr 
 * 
 * Basically the same function as build above, with the addition of
 * two new operators 
 *
 * The function is called with the argument pair (rand,depth) .
 *
 * The first element rand is a random number generator of type int * int -> int . 
 * Each call rand (i,j) returns a random integer between i inclusive and j exclusive. 
 * Use this function to randomly select operators when composing subexpressions 
 * to build up larger expressions.
 *
 * The second element depth is a maximum nesting depth. 
 * A random expression of depth d built by randomly composing sub-expressions of 
 * depth d-1 , and the only expressions of depth 0 are VarX or VarY . 
 * 
 * The two new operators are Max and Cube, both implemented in expr.ml 
 *)

let rec build2 (rand,depth) = 
 let pumping = 1+rand(0,7) in
  let alternate = rand(0, pumping) in
  if depth == 0 then (
    if alternate mod 2 == 0 then
      buildX() 
    else
      buildY()
  )
  else begin
    let x = rand(0,9) in
      let result = match x with
      |0 -> build(rand, (depth-1))
      |1 -> build(rand, (depth-1))
      |2 -> buildSine(build(rand, (depth-1))) 
      |3 -> buildCosine(build(rand, (depth - 1)))
      |4 -> buildAverage(build(rand, (depth-1)),build(rand, (depth-1)))
      |5 -> buildTimes(build(rand, (depth-1)),build(rand, (depth-1)))
      |6 -> buildMax(build(rand, (depth-1)),build(rand, (depth-1)))
      |7 -> buildCube(build(rand, (depth-1)),build(rand, (depth-1)),build(rand, (depth-1)))
      |_ -> buildThresh(build(rand, (depth-1)),build(rand, (depth-1)),build(rand, (depth-1)),build(rand, (depth-1)))
    in result
  end

(* Please fill in ALL of g1,g2,g3,c1,c2,c3 regardless of whether you
 * are aiming for extra credit. 
 *)

(* g1,g2,g3,c1,c2,c3 : unit -> int * int * int
 * these functions should return the parameters needed to create your 
 * top three color / grayscale pictures.
 * they should return (depth,seed1,seed2)
 *)

let g1 () = (8,8,20) 
let g2 () = (4,8,12)  
let g3 () = (3,9,25)

let c1 () = (10,15,12)
let c2 () = (2,20,40)
let c3 () = (5,50,70) 

(**** You should not need to modify any code below here ****)


(******************** Random Number Generators ************)

(* makeRand int * int -> (int * int -> int)
   Returns a function that, given a low and a high, returns
   a random int between the limits.  seed1 and seed2 are the
   random number seeds.  Pass the result of this function
   to build 

   Example:
      let rand = makeRand(10,39) in 
      let x =  rand(1,4) in 
          (* x is 1,2,3, or 4 *)
*)

let makeRand (seed1, seed2) = 
  let seed = (Array.of_list [seed1;seed2]) in
  let s = Random.State.make seed in
  (fun (x,y) -> (x + (Random.State.int s (y-x))))


let rec rseq g r n =
  if n <= 0 then [] else (g r)::(rseq g r (n-1))

(********************* Bitmap creation code ***************)

(* 
   You should not have to modify the remaining functions.
   Add testing code to the bottom of the file.
*)
  
(* Converts an integer i from the range [-N,N] into a float in [-1,1] *)
let toReal (i,n) = (float_of_int i) /. (float_of_int n)

(* Converts real in [-1,1] to an integer in the range [0,255]  *)
let toIntensity z = int_of_float (127.5 +. (127.5 *. z))

let ffor (low,high,f) = if low > high then () else let _ = f low in ffor (low+1,high,f)

(* emitGrayscale :  ((real * real) -> real) * int -> unit
 emitGrayscale(f, N) emits the values of the expression
 f (converted to intensity) to the file art.pgm for an 
 2N+1 by 2N+1 grid of points taken from [-1,1] x [-1,1].
 
 See "man pgm" on turing for a full description of the file format,
 but it's essentially a one-line header followed by
 one byte (representing gray value 0..255) per pixel.
 *)

let emitGrayscale (f,n,fname) =
    (* Open the output file and write the header *)
    let chan = open_out (fname^".pgm") in
    (* Picture will be 2*N+1 pixels on a side *)
    let n2p1 = n*2+1 in   
    let _ = output_string chan (Printf.sprintf "P5 %d %d 255\n" n2p1 n2p1) in
    let _ = 
      ffor (-n, n, 
        fun ix ->
          ffor (-n, n, 
            fun iy ->
              (* Convert grid locations to [-1,1] *)
              let x = toReal(ix,n) in
              let y = toReal(iy,n) in
              (* Apply the given random function *)
              let z = f (x,y) in
              (* Convert the result to a grayscale value *)
              let iz = toIntensity(z) in
              (* Emit one byte for this pixel *)
              output_char chan (char_of_int iz))) in 
    close_out chan;
    ignore(Sys.command ("convert "^fname^".pgm "^fname^".jpg"));
    ignore(Sys.command ("rm "^fname^".pgm"))

(* doRandomGray : int * int * int -> unit
 Given a depth and two seeds for the random number generator,
 create a single random expression and convert it to a
 grayscale picture with the name "art.pgm" *)

let doRandomGrayBuilder (depth,seed1,seed2,builder,prefix) =
  (* Initialize random-number generator g *)
  let g = makeRand(seed1,seed2) in
  (* Generate a random expression, and turn it into an ML function *)
  let e = builder (g,depth) in
  let _ = print_string (exprToString e) in
  let f = eval_fn e in
  (* 301 x 301 pixels *)
  let n = 150 in
  (* Emit the picture *)
  let name = Printf.sprintf "%s_%d_%d_%d" prefix depth seed1 seed2 in
  emitGrayscale (f,n,name)

let doRandomGray  (depth,seed1,seed2) = doRandomGrayBuilder (depth,seed1,seed2,build,"art_g_1")
let doRandomGray2 (depth,seed1,seed2) = doRandomGrayBuilder (depth,seed1,seed2,build2,"art_g_2")


(* emitColor : (real*real->real) * (real*real->real) *
               (real*real->real) * int -> unit
 emitColor(f1, f2, f3, N) emits the values of the expressions
 f1, f2, and f3 (converted to RGB intensities) to the output
 file art.ppm for an 2N+1 by 2N+1 grid of points taken 
 from [-1,1] x [-1,1].
 
 See "man ppm" on turing for a full description of the file format,
 but it's essentially a one-line header followed by
 three bytes (representing red, green, and blue values in the
 range 0..255) per pixel.
 *)
let emitColor (f1,f2,f3,n,fname) =
    (* Open the output file and write the header *)
    let chan = open_out (fname^".ppm") in
    (* Picture will be 2*N+1 pixels on a side *)
    let n2p1 = n*2+1 in   
    let _ = output_string chan (Printf.sprintf "P6 %d %d 255\n" n2p1 n2p1) in
    let _ = 
      ffor (-n, n, 
        fun ix ->
          ffor (-n, n, 
            fun iy ->
              (* Convert grid locations to [-1,1] *)
              let x = toReal(ix,n) in
              let y = toReal(iy,n) in
              (* Apply the given random function *)
              let z1 = f1 (x,y) in
              let z2 = f2 (x,y) in
              let z3 = f3 (x,y) in

              (* Convert the result to a grayscale value *)
              let iz1 = toIntensity(z1) in
              let iz2 = toIntensity(z2) in
              let iz3 = toIntensity(z3) in
              
              (* Emit one byte per color for this pixel *)
              output_char chan (char_of_int iz1);
              output_char chan (char_of_int iz2);
              output_char chan (char_of_int iz3);
         )) in  
    close_out chan;
    ignore(Sys.command ("convert "^fname^".ppm  "^fname^".jpg"));
    ignore(Sys.command ("rm "^fname^".ppm")) 

(* doRandomColor : int * int * int -> unit
 Given a depth and two seeds for the random number generator,
 create a single random expression and convert it to a
 color picture with the name "art.ppm"  (note the different
 extension from toGray) 
 *)
let doRandomColorBuilder (depth,seed1,seed2,builder,prefix) =
  (* Initialize random-number generator g *)
  let g = makeRand (seed1,seed2) in
  (* Generate a random expression, and turn it into an ML function *)
  let e1 = builder (g, depth) in
  let e2 = builder (g, depth) in
  let e3 = builder (g, depth) in
  
  let _ = Printf.printf "red   = %s \n" (exprToString e1) in
  let _ = Printf.printf "green = %s \n" (exprToString e2) in
  let _ = Printf.printf "blue  = %s \n" (exprToString e3) in

  let f1 = eval_fn e1 in
  let f2 = eval_fn e2 in
  let f3 = eval_fn e3 in

  (* 301 x 301 pixels *)
  let n = 150 in
  (* Emit the picture *)
  let name = Printf.sprintf "%s_%d_%d_%d" prefix depth seed1 seed2 in
  emitColor (f1,f2,f3,n,name)

let doRandomColor  (depth,seed1,seed2) = doRandomColorBuilder (depth,seed1,seed2,build,"art_c_1")
let doRandomColor2 (depth,seed1,seed2) = doRandomColorBuilder (depth,seed1,seed2,build2,"art_c_2")
  
(*************** Insert Testing Code Here ******************)
