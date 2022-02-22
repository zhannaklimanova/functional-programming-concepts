exception NotImplemented
let domain () =
    failwith "REMINDER: You should not be writing tests for undefined values."


(*----------------------------------------------------------------*)
(* SECTION 1: String to Characters to String- Function tabulate *)
(* tabulate f n returns [f 0; f 1; ...; f (n - 1)]                *)
(*----------------------------------------------------------------*)

let rec tabulate f n =
  let rec tab n acc =
    if n < 0 then acc
    else tab (n - 1) ((f n) :: acc)
  in
  tab (n - 1) []

(*---------------------------------------------------*)
(* SECTION 2 : Unfolding is like folding in reverse *)
(*---------------------------------------------------*)

let rec unfold (f : 'seed -> 'a * 'seed) (stop : 'seed -> bool) (b : 'seed) : 'a list =
  if stop b then
    []
  else
    let (x, b') = f b in
    x :: unfold f stop b'

(*
 Example function using `unfold`. Generates a list of natural numbers less than `max`.
*)
let nats max = unfold (fun b -> (b, b+1)) (fun b -> max <= b) 0

(*----------------------------------------*)  
(* SECTION 3 : Let's *safely* have cake! *)
(*----------------------------------------*)

type price = float
type weight = float
type calories = int
type ingredient = Nuts | Gluten | Soy | Dairy
type cupcake = Cupcake of price * weight * calories * ingredient list

(* Example Cupcakes and cupcake list.*)
let c1 = Cupcake (2.5, 80.3, 250, [Dairy; Nuts])
let c2 = Cupcake (2.75, 90.5, 275, [Dairy; Soy])
let c3 = Cupcake (3.05, 100.4, 303, [Dairy; Gluten; Nuts])
let c4 = Cupcake (3.25, 120.4, 330, [Dairy; Gluten ])
let cupcakes = [c1 ; c2 ; c3 ; c4]

(*--------------------------------------------------------------*)
(* SECTION 1 : String to Characters to String                   *)
(*--------------------------------------------------------------*)

(* 
 string_explode : string -> char list turns a string into a list of characters. 
*)
let string_explode (s : string) : char list = 
  tabulate (String.get s) (String.length s)

(*
 string_implode : char list -> string turns a list of characters into a string.
*)
let string_implode (l : char list) : string =
  List.fold_right (fun a -> (^) (Char.escaped a)) l ""

(*--------------------------------------------------------------*)    
(* SECTION 2: unfolding is like folding in reverse              *)
(*--------------------------------------------------------------*)
                     
(* 
 evens : int -> int list computes the even natural numbers (successive even 
 integers) up to an exclusive limit max.
*)
let evens (max : int) : int list =
  unfold 
    (fun b -> (b, 2 + b)) 
    ((<=) max) 
    0

(* 
 fib : int -> int list computes the Fibonacci sequence up to an exclusive limit. 
*)
let fib (max : int) : int list =
  unfold 
    (fun (a, b) -> a, (b, a + b)) 
    (fun (a, b) -> max <= a) 
    (1, 1)
    
(*
 pascal : int -> int list list computes Pascal's triangle up to a maximum row 
 length. Where Pascal's triangle is a number triangle with numbers arranged in 
 staggered rows such that each number in the triangle is the sum of the two 
 numbers above it. 
*)
let pascal (max : int) : int list list =
  unfold 
    (fun list -> (list, List.map2 (+) (0 :: list) (list @ [0]))) 
    (fun list -> max + 1 <= List.length list) 
    [1] 
  
(* 
 zip : 'a list -> 'b list -> ('a * 'b) list converts two lists into a list of 
 tuples. This implementation uses pattern matching.
 e.g. zip [1; 2] ['a'; 'c'] = [(1, 'a'); (2, 'c')]
 e.g. zip [1; 2] ['a'] = [(1, 'a')]

 Note: if one list is shorter than the other, then the 
 resulting list has the length of the smaller list.     
*) 
let zip (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  unfold 
    (fun (a, b) -> 
       match (a, b) with 
       | ([], _) -> failwith "Empty input lists are handled by unfold."
       | (_, []) -> failwith "Empty input lists are handled by unfold."
       | (h1 :: t1, h2 :: t2) -> ((h1, h2), (t1, t2)))
    (fun (a, b) -> a = [] || b = [])
    (l1, l2)
  
(* 
 zip2 : 'a list -> 'b list -> ('a * 'b) list converts two lists into a list of 
 tuples.
 e.g. zip [1; 2] ['a'; 'c'] = [(1, 'a'); (2, 'c')]
 e.g. zip [1; 2] ['a'] = [(1, 'a')]

 Note: if one list is shorter than the other, then the 
 resulting list has the length of the smaller list.     
*) 

let zip2 (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  if l1 = [] || l2 = [] then []
  else
    unfold 
      (fun (c, (a, b)) -> 
         if c + 1 >= (min (List.length l1) (List.length l2)) then 
           ((List.nth l1 (c-1), List.nth l2 (c-1)), (c + 1, (List.nth l1 (c-1), List.nth l2 (c-1)))) 
         else ((a, b), (c + 1, (List.nth l1 (c), List.nth l2 (c)))))
      (fun (c, (a, b)) -> c > (min (List.length l1) (List.length l2)))
      (1, (List.hd l1, List.hd l2)) 
  
      
(*--------------------------------------------------------------*)
(* SECTION 3 : Let's *safely* have cake!                        *)
(*--------------------------------------------------------------*)

(*
 allergy_free : ingredient list -> cupcake list -> cupcake list returns the 
 cupcakes from the cupcake list that contain none of the allergens.
 
 e.g. INPUT:
 allergy_free
  [Dairy; Gluten; Soy]
  [Cupcake (3.42, 89.8, 316, [Soy]);
   Cupcake (1.67, 86.48, 312, [Gluten; Soy; Nuts; Dairy]);
   Cupcake (0.03, 91.65, 291, [Nuts]);
   Cupcake (0.09, 86.3, 269, [Dairy; Soy; Gluten; Nuts]);
   Cupcake (3.93, 108.16, 331, [Dairy; Soy]);
   Cupcake (4.96, 85.86, 316, [Nuts]);
   Cupcake (2.45, 83.4, 263, [Dairy; Gluten; Soy]);
   Cupcake (3.51, 96.18, 285, [Gluten])]

 e.g. OUTPUT
 [Cupcake (0.03, 91.65, 291, [Nuts]); 
  Cupcake (4.96, 85.86, 316, [Nuts])]
*) 
let allergy_free (allergens : ingredient list) (cupcakes : cupcake list) : cupcake list =
  List.filter (fun cupcake -> 
      let Cupcake (_,_,_,ingredients) = cupcake in 
      (List.for_all (fun ingredient -> 
           not (List.exists ((=) ingredient) allergens)) 
          ingredients))
    cupcakes 

    