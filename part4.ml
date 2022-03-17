(* ------------------------------------------------------------------------*)
(* Part 1 : Money in the Bank *)
(* ------------------------------------------------------------------------*)

(*
  open_account : passwd -> bank_account takes an initial password as an argument
  and creates a bank account that stores the password, the current balance of
  the account (initially 0), and provides four methods, with signatures as 
  indicated in the bank_account type.
  
  - update_pass: takes the old password as the first argument and the new password 
  as the second argument, and updates the stored password if the provided old 
  password is correct. It does not check if the old password is the same as the 
  new password.
  
  - deposit allows a user to deposit money into the account, if the user provides 
  the correct password.
  
  - retrieve allows a user to retrieve money from the account, if the user provides 
  the correct password and there are sufficient funds available in the account.

  - show_balance allows the user to query the account balance, if the user provides 
  the correct password.
*)
let open_account (initial_pass: passwd) : bank_account =
  let currentPassword = ref initial_pass in 
  let currentBalance = ref 0 in
  let signInAttempt = ref 0 in
  
  {
    update_pass = (fun password newPassword -> 
        if password <> !currentPassword then
          let _ = incr signInAttempt in
          raise wrong_pass
        else
          let _ = signInAttempt := 0 in
          currentPassword := newPassword; ());
    
    retrieve = (fun password amount-> 
        if !signInAttempt < 5 then
          if password <> !currentPassword then
            let _ = incr signInAttempt in 
            raise wrong_pass
          else 
            let _ = signInAttempt := 0 in
            if amount < 0 then raise negative_amount
            else if amount > !currentBalance then raise not_enough_balance 
            else 
              let _ = currentBalance := !currentBalance - amount in
              ()
        else raise too_many_failures);
    
    deposit = (fun password -> fun amount -> 
        if !signInAttempt < 5 then
          if password <> !currentPassword then
            let _ = incr signInAttempt in 
            raise wrong_pass
          else 
            let _ = signInAttempt := 0 in
            if amount < 0 then raise negative_amount 
            else 
              let _ = currentBalance := !currentBalance + amount in
              ()
        else raise too_many_failures);
    
    show_balance = fun password -> 
      if !signInAttempt < 5 then
        if password <> !currentPassword then
          let _ = incr signInAttempt in 
          raise wrong_pass
        else
          let _ = signInAttempt := 0 in
          !currentBalance 
      else raise too_many_failures
  }
;;

(* ------------------------------------------------------------------------*)
(* Part 2 : I Want to Travel *)
(* ------------------------------------------------------------------------*)

let g = {nodes = []; 
         edges = []
        };;
let g1 = {nodes = ["v1"]; 
          edges = []
         };;
let g2 = {nodes = ["v1"; "v2"]; 
          edges = [("v1", "v2", 7)]
         };;
let g3 = {nodes = ["v1"; "v2"; "v3"]; 
          edges = [("v1", "v2", 3); ("v1", "v3", 4)]
         };;
let g4 = {nodes = ["v1"; "v2"; "v3"; "v4"]; 
          edges = [("v1", "v2", 1); ("v1", "v3", 2); 
                   ("v3", "v2", 3);
                   ("v2", "v4", 4)]
         };;
let g5 = {nodes = ["v1"; "v2"; "v3"; "v4"; "v5"; "v6"; "v7"]; 
          edges = [("v1", "v3", 9); ("v1", "v2", 5); 
                   ("v3", "v4", 7);
                   ("v4", "v2", 2); ("v4", "v5", 1); ("v4", "v6", 12);
                   ("v6", "v7", 4)]
         };;

(*
  Test cases for the neighbours : 'a graph -> 'a -> ('a * int) list function.
  
  Note: 
  It is assumed that input graphs are well-formed (nodes list will be a list of
  all the nodes in the graph, and no other values wll appear in the edge pairs.

  It is possible for a node in the graph to not be connected to any other nodes. 
  
  There will be no duplicate nodes or edges, and no self-loops.

*)
let neighbours_tests: ((string graph * string) * (string * weight) list) list = [
  ((g, ""), ([]));
  ((g1, "v1"), ([]));
  ((g2, "v1"), ([("v2", 7)]));
  ((g2, "v2"), ([]));
  ((g3, "v1"), ([("v2", 3); ("v3", 4)]));
  ((g3, "v3"), ([]));
  ((g4, "v3"), ([("v2", 3)]));
  ((g4, "v2"), ([("v4", 4)]));
  ((g5, "v2"), ([]));
  ((g5, "v1"), ([("v3", 9); ("v2", 5)]));
  ((g5, "v4"), ([("v5", 1); ("v2", 2); ("v6", 12)])); 
  ((g5, "v6"), ([("v7", 4)])); 
]
;;

(* 
  neighbours : 'a graph -> 'a -> ('a * int) list returns a list of the input 
  vertex's (node's) out-neighbours together with the cost it takes to reach each
  out neighbour node.
   
  e.g.
  A node v2 is an out-neighbour of node v1 if an edge (v1, v2, w) exists in the   
  graph, i.e., if there exists a directed edge from v1 to v2 with label w.
*)
let neighbours (g: 'a graph) (vertex: 'a) : ('a * weight) list =
  let {nodes = _; edges = edgesList } = g in
  List.fold_left
    (fun listAcc (startV, endV, weight) -> 
       if startV = vertex then (listAcc @ [(endV, weight)]) else listAcc)
    []
    edgesList
;;

(* 
  find_path : 'a graph -> 'a -> 'a -> 'a list * int which when given a graph g 
  and two nodes a and b in the graph, returns any one path (a list of nodes to visit,
  in order) from a to b together with the total cost to get to b from a. This 
  path includes both endpoints a and b. The implementation is done with 
  backtracking with the Fail exception. 
  
  Note:
  The path returned does not contain any cycles, so no node should appear in a 
  path more than once. This is done by keeping track of a list of visited nodes.
*)
let find_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node (node: 'a * weight) (visited : 'a list) : ('a list * weight) = 
    if (fst node) = b then ([fst node], snd node)
    else if List.mem (fst node) visited then raise Fail (* backtrack if cycle *)
    else let (nodePath, totalCost) = 
           aux_list (neighbours g (fst node)) ((fst node) :: visited) in 
      ((fst node) :: nodePath, (snd node) + totalCost)
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) =
    match nodes with 
    | [] -> raise Fail
    | h :: t -> 
        try 
          aux_node h visited
        with Fail -> aux_list t visited
  in
  aux_node (a, 0) []

(* 
  find_path' : 'a graph -> 'a -> 'a -> 'a list * int returns a path from node a 
  to b, or raises the exception Fail if no path exists. The function is implemented
  tail-recursively using continuations. 

  Note:
  The path returned does not contain any cycles, so no node should appear in a 
  path more than once. This is done by keeping track of a list of visited nodes.
*)
let find_path' (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node (node: 'a * weight) (visited : 'a list) fc sc : ('a list * weight)=
    if (fst node) = b then sc ([fst node], snd node)
    else if List.mem (fst node) visited then fc ()
    else let sc' = 
           (fun (path, cost) -> sc ((fst node) :: path, (snd node) + cost)) in 
      aux_list (neighbours g (fst node)) ((fst node) :: visited) fc sc'
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) fc sc : ('a list * weight) =
    match nodes with 
    | [] -> fc ()
    | h :: t -> 
        let sc' = sc in
        let fc' = fun () -> aux_list t visited fc sc in
        aux_node h visited fc' sc'
  in
  aux_node (a, 0) [] (fun () -> raise Fail) (fun s -> s)


(* 
  find_all_paths : 'a graph -> 'a -> 'a -> ('a list * int) list returns all the 
  paths from a to b together with their associated cost. If no path exists, then
  an empty list is returned.
*)
let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) list =
  let rec aux_node (node: 'a * weight) (visited : 'a list) : ('a list * weight) list =
    if (fst node) = b then [([(fst node)], (snd node))]
    else if List.mem (fst node) visited then raise Fail
    else let allPathsList = 
           aux_list (neighbours g (fst node)) ((fst node) :: visited) in
      (* Put each node of the path into the list of all paths and associated weights *)
      List.map 
        (fun (eachNodePath, eachTotalCost) -> 
           ((fst node) :: eachNodePath, (snd node) + eachTotalCost) )
        allPathsList 
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) list =
    match nodes with 
    | [] -> [] (* if no path exists *)
    | h :: t -> 
        try 
          aux_node h visited @ aux_list t visited
        with Fail -> aux_list t visited
  in
  aux_node (a, 0) []


(*
  find_longest_path : 'a graph -> 'a -> 'a -> ('a list * int) option returns the
  highest cost path from a to b with its associated cost, or returns None if no 
  path exists.
*)
let find_longest_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) option =
  let rec findLongest allPaths longestPath longestLen : ('a list * weight) option = 
    match allPaths with
    | [] -> Some longestPath
    | (path, w) :: t -> 
        if w > longestLen then findLongest t (path, w) w
        else findLongest t longestPath longestLen
  in 
  let paths = find_all_paths g a b in 
  if (List.length paths) = 0 then None
  else 
    findLongest paths ([], 0) (-1)

