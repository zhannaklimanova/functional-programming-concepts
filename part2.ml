(* SECTION 2 type definitions. *)

type dist_unit = Foot | Meter | Mile
type time_unit = Second | Hour
type speed_unit = dist_unit * time_unit
type 'a value = 'a * float

(* SECTION 3 type definitions. *)

type tree = Branch of float * tree list | Leaf


(* SECTION 1 : Lists *)

(* 
 Test cases for the mode : 'a list -> 'a function.
*)
(*
let mode_tests: (int list * int) list = [
  (([1; 2; 3; 4; 5]), 1);
  (([5; 4; 3; 2; 1]), 1);
  (([1; 1; 1; 1; 1]), 1);
  (([1; 2; 3; 4; 4; 4; 3; 2; 2]), 2);
  (([1; 2; 3; 4; 4; 4; 3; 2]), 4);
  (([5; 4; 6; 5; 4; 2; 5; 4; 2; 1]), 4); 
  (([11; 5; 8; 19; 7; 3; 2; 4; 1]), 1);
  (([1; 2; 3; 2; 3]), 2);
  (([1; 3]), 1);
  (([1; 2; 2; 3; 2; 2]), 2);
  (([11; 5; 8; 19; 7; 3; 2; 4; 5; 1]), 5);
  (([6; 2; 6; 2; 3]), 2);
  (([1; 2]), 1);
  (([2; 2]), 2);
  (([0]), 0)
] ;;
*)


(* 
 mode : 'a list -> 'a takes a list containing any type ('a) elements and returns
 the most common one (mode of the distribution represented by the list). In the 
 case of two or more elements that appear the same number of times, the smallest
 element is returned.
*)
let mode (l: 'a list) : 'a =
  let rec aux l ((cur_el, cur_num) : 'a * int) ((max_el, max_num) : 'a * int) =
    match l with 
    | [] -> failwith "Undefined input."
    | [_] -> max_el 
    | h :: t -> 
        if h <> (List.nth t 0) then aux t (List.nth t 0, 1) (max_el, max_num)
        else 
          let cur_num' = cur_num + 1 in 
          let max_el' = if cur_num' > max_num then cur_el else max_el in 
          let max_num' = if cur_num' > max_num then cur_num' else max_num in 
          aux t (cur_el, cur_num') (max_el', max_num') 
  in
  let sortedList = List.sort compare l in
  aux sortedList (List.hd sortedList, 1) (List.hd sortedList, 1)
    
    
(* 
 Test cases for the pair_mode : 'a list -> 'a * 'a function.
*)
(*
let pair_mode_tests: (int list * (int * int) ) list = [
  (([1; 2; 3; 2; 3]), (2, 3));
  (([1; 3]), (1, 3));
  (([1; 2; 2; 3; 2; 2]), (2, 2));
  (([11; 5; 8; 19; 7; 3; 2; 4; 5; 1]), (2, 4));
  (([6; 2; 6; 2; 3]), (6, 2));
  (([9; 9; 9; 9; 9]), (9, 9))
] ;;
*)

(* 
 pair_mode : 'a list -> 'a * 'a takes any list and returns the most common pair
 pair of consecutive elements (or bi-gram) in the list.
*)
let pair_mode (l: 'a list) : 'a * 'a = 
  let rec recurse l acclist= match l with 
    | [] -> mode (List.rev (acclist))
    | [_] -> mode (List.rev (acclist))
    | h :: t :: [] -> mode (List.rev ((h, t) :: acclist))
    | h :: h2 :: t -> recurse (h2 :: t) ((h, h2) :: acclist)
  in 
  if List.length l = 0 || List.length l = 1 
  then failwith "Undefined input: list length should be >= 2."
  else recurse l [] 

(* SECTION 2 : Custom data types *)

(* 
 convert_time : time_unit value -> time_unit -> time_unit value performs time 
 convertions between seconds and hours; where val_ >= 0.
*)
let convert_time ((from_unit, val_) : time_unit value) to_unit : time_unit value = 
  if val_ < 0. then failwith "Time values cannot be negative."
  else 
    match (from_unit, val_) with 
    | (Hour, _) ->
        (match to_unit with
         | Hour -> (Hour, val_)
         | Second -> (Second, val_ *. 3600.0))
    | (Second, _) ->
        (match to_unit with
         | Hour -> (Hour, val_ /. 3600.0)
         | Second -> (Second, val_)) 
                                     
(* 
 convert_dis : dist_unit value -> dist_unit -> dist_unit value performs distance 
 convertions between feet, miles, and meters; where val_ >= 0.
*)
let convert_dist ((from_unit, val_) : dist_unit value) to_unit : dist_unit value =
  if val_ < 0. then failwith "Distance values cannot be negative."
  else 
    match (from_unit, val_) with 
    | (Foot, _) ->
        (match to_unit with
         | Foot -> (Foot, val_)
         | Mile -> (Mile, (val_ /. 5280.0))
         | Meter -> (Meter, (val_ *. 0.3048))) 
    | (Mile, _) ->
        (match to_unit with
         | Foot -> (Foot, (val_ *. 5280.0) )
         | Mile -> (Mile, val_)
         | Meter -> (Meter, (val_ *. 1609.344)))
    | (Meter, _) ->
        (match to_unit with
         | Foot -> (Foot, (val_ /. 0.3048))
         | Mile -> (Mile, (val_ /. 1609.344))
         | Meter -> (Meter, val_))
  
(* 
 convert_speed : speed_unit value -> speed_unit -> speed_unit value performs 
 speed convertions of various units; where val_ >= 0.
*)
let convert_speed ((from_unit, val_) : speed_unit value) to_unit : speed_unit value =
  if val_ < 0. then failwith "Speed values cannot be negative."
  else 
    let distance' = (convert_dist (fst from_unit, val_) (fst to_unit)) in 
    let time' = (convert_time (snd from_unit, 1.0) (snd to_unit)) in
    let timeMult = 1.0 /. (snd time') in
    let speed' = (fst distance', fst time') in
    (speed', (snd distance') *. timeMult)

(* 
 add_speed : speed_unit value -> speed_unit value -> speed_unit value adds two 
 speed values together and returns the output in the unit of the second argument.
*)
let add_speed (a : speed_unit value) ((b_unit, b_val) : speed_unit value) : speed_unit value = 
  let speed' = convert_speed a b_unit in
  (b_unit, (snd speed') +. b_val) 

(* 
 dist_traveled : time_unit value -> speed_unit value -> dist_unit value 
 calculates the distance traveled if the speed given was maintained for the time
 period provided. 
*)
let dist_traveled time ((speed_unit, speed_val) : speed_unit value) : dist_unit value = 
  let time' = convert_time time (snd speed_unit) in
  ((fst speed_unit), (snd time' *. speed_val))

  
(* SECTION 3 : recursive data types/induction *)

let tree0 = Leaf ;;
let tree1 = Branch (4., [Leaf]) ;;
let tree2 = Branch (4., [Branch (3., [Leaf])]) ;; 
let tree3 = Branch (4., [Branch (5., [Leaf])]) ;;
let tree4 = Branch (-6., [Branch (5., [Leaf])]) ;;
let tree5 = Branch (0., [Branch (0., [Leaf])]) ;;
let tree6 = Branch (5., [Branch (3., [Leaf; Leaf; Leaf]);
                         Branch (2., [Branch (2., [Leaf])]);
                         Branch (4., [])]) ;;
let tree7 = Branch (4., []) ;; 
let tree8 = Branch (3., [Branch (3., [Branch (2., []); Leaf; Leaf])]) ;;
let tree9 = (Branch (51.9977478483348321,
              [Branch (20.7138282542315046,
                [Branch (3.9847785029833318,
                  [Leaf; Leaf; Branch (2.21814703383210654, [Leaf]); Leaf]);
                Branch (6.83920483396512,
                  [Branch (4.09638867093755188, [Leaf; Leaf])]);
                Branch (14.014974610314086,
                  [Branch (1.12056568624224839, [Leaf; Leaf]);
                  Branch (9.70365780434576308,
                    [Branch (2.13209657782730311, [Leaf; Leaf; Leaf]);
                    Branch (3.80544824968402784, [Leaf; Leaf; Leaf]);
                    Branch (2.48090433842469649, [Leaf])]);
                  Leaf]);
                Leaf]);
              Branch (7.52830335185880628,
                [Leaf; Branch (0.245898410221691566, [Leaf; Leaf; Leaf]);
                Branch (5.54128479998886814,
                  [Branch (2.17799234705562661, [Branch (1.52258552253849855, [Leaf])]);
                  Branch (2.7263467062697635, [Leaf; Leaf])])]);
              Branch (36.0843586490757886,
                [Branch (0.466370886624081771, [Leaf; Leaf]);
                Branch (3.64682851673227759,
                  [Branch (1.04686264816911, [Leaf; Leaf]); Leaf;
                  Branch (1.43574568590117879, [Leaf; Leaf])]);
                Branch (26.7659068171790224,
                  [Branch (5.49349739004023352,
                    [Leaf; Branch (1.43494094978208575, [Leaf; Leaf; Leaf; Leaf]);
                    Branch (4.41839898215638183, [Leaf])])])]);
              Branch (12.1508271144725644,
                [Branch (2.88674128081462555,
                  [Branch (0.665729850487968511,
                    [Branch (0.210391625355443534, [Leaf; Leaf; Leaf; Leaf]); Leaf])]);
                Branch (3.63203730459603147, [Branch (1.81172364514186368, [Leaf])]);
                Branch (8.5857946467734525,
                  [Leaf;
                  Branch (3.11315246377692789,
                    [Branch (1.80921182376222078, [Leaf]);
                    Branch (1.50907272058893782, [Leaf; Leaf; Leaf; Leaf]); Leaf]);
                  Leaf])])]));;
let tree10 = (Branch (96.9458357991820776,
                [Branch (51.9860666792418229,
                [Branch (59.917700686521556,
                  [Branch (68.9111188804677113, [Leaf; Leaf; Leaf])]);
                  Leaf]);
                Branch (128.423230075951238,
                [Branch (70.1466013460413791,
                  [Branch (76.2351592194399,
                    [Branch (56.241344079065712, [Leaf; Leaf])])])])]));;
let tree11 = (Branch (46.074634817048981,
                [Branch (12.1274715435179, [Leaf]);
                Branch (2.9735111669657921,
                  [Branch (1.15865187234968703, [Leaf; Leaf]);
                  Branch (1.76113012363069599, [Leaf; Leaf])]);
                Branch (26.3195487217758277,
                  [Leaf; Branch (5.22567184801709583, [Leaf])]);
                Branch (3.82379922022979457,
                  [Branch (2.19284421818360542, [Leaf; Leaf])])]));;
(* 
 Test cases for the passes_da_vinci: tree -> bool function.
*)
(*
let passes_da_vinci_tests : (tree * bool) list = [ 
  (tree0, true);
  (tree1, true);
  (tree2, true);
  (tree3, false);
  (tree4, true);
  (tree5, true);
  (tree6, false);
  (tree7, true); 
  (tree8, true);
  (tree9, true);
  (tree10, false);
  (tree11, true)
] ;;
*)

(* 
 leafCounter : tree -> int finds the total number of leaves in a tree. 
*)
let leafCounter t = 
  let rec countLeavesInTree (t: tree) = match t with 
    | Leaf -> 1
    | Branch (width, subtrees) -> countLeavesInTreeList subtrees
  and countLeavesInTreeList b = match b with 
    | [] -> 0
    | h :: t -> (countLeavesInTree h) + (countLeavesInTreeList t)
  in 
  countLeavesInTree t 
                      
(* 
 leafCounterTailRecursive : tree -> int finds the total number of leaves in a 
 tree. This function is optimized with tail recursion. 
*)
let leafCounterTailRecursive t = 
  let rec countLeavesInTree (t: tree) acc = match t with 
    | Leaf -> 1 + acc
    | Branch (width, subtrees) -> countLeavesInTreeList subtrees acc
  and countLeavesInTreeList b acc = match b with 
    | [] -> acc 
    | head :: tail ->
        let acc' = countLeavesInTree head acc in
        countLeavesInTreeList tail acc'
  in 
  countLeavesInTree t 0
           
(* 
 findSubtreeSum : tree list -> int finds the sum of squares of the immediate
 tree node children.
*)
let findSubtreeSum (subtree: tree list)  =                                
  let rec findSumSquares (subtree: tree list) acc = match subtree with 
    | [] -> acc
    | Leaf :: t -> findSumSquares t acc
    | Branch (width, _) :: t -> 
        let acc' = (width ** 2.) +. acc in
        findSumSquares t acc'
  in findSumSquares subtree 0.
  
(*
passes_da_vinci primer:
Leonardo da Vinci (1452-1519) was one of the greatest minds to ever live. 
Between all his accomplishments, da Vinci has made an observation about trees (the ones outside). 
A rephrasing of his original statement is that the sum of surface areas of all child branches 
is equal to the surface area of the parent branch. In 2014, researchers from the 
University of Tokyo showed that this is linked to, although not in such a simple relationship, 
tension and stress experienced by the plants during growth.
*)

(* 
 passes_da_vinci : tree -> bool verifies for each branch within the tree that 
 the sum of squares of a node's immediate child branches widths is less than or 
 equal to the square of the node's branch's width. 

 Note: 
 The ttf inner method stands for tree_traversal_function and traverses the 
 subtrees of the entire tree. 
 
 The ctf inner method stands for child_traversal_function and traverses the list
 of subtrees. 
*)
let passes_da_vinci t = 
  let rec ttf (t: tree) = match t with 
    | Leaf -> true
    | Branch (width, subtrees) -> 
        if (width ** 2.) >= findSubtreeSum subtrees then ctf subtrees
        else false
  and ctf subtreeList = match subtreeList with 
    | [] -> true 
    | h :: t -> ttf h && ctf t
  in 
  ttf t 
    
