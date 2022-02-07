exception NotImplemented
let domain () =
    failwith "REMINDER: You should not be writing tests for undefined values."
    
(* 
  Test cases for the fact : int -> float function.
  It is assumed that n will not be greater than 0.
*)
(*
let fact_tests = [
  (0, 1.);
  (1, 1.);
  (2, 2.);
  (5, 120.0);
  (13, 6227020800.0);
  (20, 2432902008176640000.0)
] ;;
*)

(* 
  fact : int -> float calculates the factorial of a number n; where n >= 0.
*)
let rec fact (n: int): float = match n with
  | 0 -> 1.
  | _ -> float_of_int (n) *. fact (n - 1)


(* 
  Test cases for the binomial : int -> int -> float function.
  It is assumed that n >= k >= 0.
*)
(*
let binomial_tests = [ 
  ((0, 0), 1.);
  ((1, 0), 1.); 
  ((1, 1), 1.);
  ((2, 0), 1.);
  ((2, 2), 1.); 
  ((7, 7), 1.);
  ((10, 1), 10.); 
  ((8, 7), 8.);
  ((10, 2), 45.)
] ;;
*)

(* 
  binomial : int -> int -> float calculates the binomial coefficient;
  where n >= k >= 0.
*)
let binomial (n: int) (k: int) =
  if n < 0 || k < 0 then domain () 
  else (if k > n then domain ()
        else fact (n) /. (fact (k) *. fact (n - k)))


(* 
  Test cases for the distance : (int * int) -> (int * int) -> float function.
*)
(*
let distance_tests = [ (* write more test cases for testing all quadrants *)
  (((0, 0), (0, 0)), 0.0);
  (((1, 1), (1, 1)), 0.0);
  (((-3, -2), (14, 5)), 18.384776);
  (((-3, -7), (-3, 6)), 13.0);
  (((3, 8), (6, 5)), 4.242641);
  (((2, -8), (6, -8)), 4.0);
  (((-8, -1), (-6, -4)), 3.605551);
  (((-8, 5), (-6, 6)), 2.236068);
  (((-2, 4), (7, 4)), 9.0);
  (((-9, -9), (9, 9)), 25.455844)
] ;;
*)

(* 
  distance : (int * int) -> (int * int) -> float computes the Euclidean distance
  between two points.
*)
let distance ((x1, y1): (int * int)) ((x2, y2): (int * int)) : float =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  sqrt (float_of_int ((dx * dx) + (dy * dy)))

    
(* 
  Test cases for the ackerman : int * int -> int function. 
*)
(*
let ackerman_tests = [ 
  ((1,2), 4);
  ((2,1), 5);
  ((2,2), 7);
  ((3,7), 1021);
  ((1,1), 3);
  ((0,1), 2);
  ((0,0), 1); (* n = 0, k > 0 *)
  ((4,0), 13); (* n > 0, k = 0 *)
  ((3,3), 61); (* n > 0, k > 0 *) 
  ((1,0), 2);
  ((3,10), 8189) 
] ;;
*)

(* 
  ackerman : int * int -> int; where n >= 0 and k >= 0.
*)
let ackerman (n, k)  =
  if n < 0 || k < 0 then domain ()
  else (let rec ack n k =
          match (n, k) with
          | (0, _) -> k + 1
          | (_, 0) -> ack (n-1) (1)
          | (_, _) -> ack (n-1) (ack n (k - 1))
        in ack n k) 
     

(* 
  Test cases for the is_prime : int -> bool function. 
*)
(*
let is_prime_tests = [ 
  (100, false);
  (2, true);
  (3, true); 
  (11, true);
  (13, true);
  (15, false);
  (74, false);
  (77, false);
  (100, false);
  (263, true);
  (181, true);
  (271, true); 
  (991, true); 
  (9409, false); 
  (62433, false); 
  (14351, false); 
  (10000021, false);
  (10000022, false); 
  (15485863, true)
] ;;
*)

(* 
  is_prime : int -> bool tests whether the input value is prime; where n > 1.
*)
let is_prime n = 
  let rec prime n x = match n with
    | 2 -> true
    | _ ->
        if n mod x = 0 then false
        else if x * x <= n then let x' = x + 1 in prime (n) (x')
        else true
  in 
  if n <= 1 then domain ()
  else prime n 2


(* 
  Test cases for the Reimann zeta function: float -> bool function. 
*)
(*
let zeta_tests = [
  (10.0, 1.0009945751278180853371459589003190170060195315644775172577889946);
  (5.0, 1.0369277551433699263313654864570341680570809195019128119741926779);
  (3.3, 1.15194);
  (3.0, 1.2020569031595942853997381615114499907649862923404988817922715553);
  (2.12222, 1.54358);
  (2.0, 1.6449340668482264364724151666460251892189499012067984377355582293)
] ;;
*)

(* 
  zeta : float -> float is an approximation of the Reimann zeta function which 
  computes the infinite sum of n^-k; where n > 1 and k >= 2.
*)
let zeta (k: float) : float = 
  let rec approx_zeta k acc n sum_so_far = 
    if let sum_so_far_next = (sum_so_far +. (1. /. ((n +. 1.) ** k))) 
      in sum_so_far_next -. sum_so_far < acc then (sum_so_far +. (1. /. ((n +. 1.) ** k))) 
    else let n' = n +. 1. 
      in approx_zeta (k) (acc) (n') (sum_so_far +. (1. /. (n ** k))) 
  in 
  if k < 2. then domain () 
  else approx_zeta k epsilon_float 1. 0.

   
 (* 
  Test cases for the square_root : float -> float function.  
*)
(*
let square_root_tests = [ 
  (1.0, 1.0);
  (4.0, 2.0);
  (9.0, 3.0);
  (25.0, 5.0);
  (3.0, 1.732);
  (100.99, 10.0494);
  (15.0, 3.873);
  (199999.326541, 447.21284)
] ;;
*)
  
(* 
  square_root : float -> float function approximates the square root of a 
  number using the Newton-Raphson method; where a > 0. 
*)
let square_root a =
  let rec findroot x acc =
    if (abs_float((((a /. x) +. x) /. 2.0) -. x) < acc) 
    then abs_float((((a /. x) +. x) /. 2.0)) (* base case *) 
    else 
      let x_prime = (((a /. x) +. x) /. 2.0)  
      in findroot (x_prime) (acc)
  in
  if a > 0.
  then findroot 1. epsilon_float
  else domain ()
  
  
(* 
  Test cases for the fib_tl : int -> int function.
*)
(*
let fib_tl_tests = [ 
  (0, 1);
  (1, 1);
  (2, 2);
  (3, 3);
  (4, 5);
  (5, 8);
  (6, 13); 
  (11, 144);
  (12, 233);
  (14, 610)
] ;;
*)

(* 
  fib_aux : int -> int -> int -> int calculates the fibonacci value at index n.
*)
let rec fib_aux n a b = 
  if n = 0 then a
  else let n' = n - 1 in fib_aux (n') (b) (a+b)
  
(* 
  fib_tl : int -> int calls fib_aux to calculate the Fibonacci value at index
  n; where n >= 0. 
*)
let fib_tl n =
  if n < 0 then domain ()
  else fib_aux n 1 1


