exception NotImplemented

(*
A DIY programming language called MiniCAML, in OCaml. It explores
concepts such as free variables, substitution, evaluation, type checking, 
and type inference. 
*)

(* Types in MiniCAML *)
type tp =
  | Arrow of tp list * tp   (* function type: S1 S2 ... Sn -> T *)
  | Int
  | Bool

(* Used for variables, aka "identifiers" *)
type name = string

(* The primitive operations available in MiniCAML *)
type primop = Equals | LessThan | Plus | Minus | Times | Negate

(* Expressions in MiniCAML *)
type exp =
  | I of int                          (* 0 | 1 | 2 | ... *)
  | B of bool                         (* true | false *)
  | If of exp * exp * exp             (* if e then e1 else e2 *)
  | Primop of primop * exp list       (* e1 <op> e2  or <op> e *)
  | Fn of (name * tp) list * exp      (* fn (x_1: t_1, ..., x_n: t_n) => e *)
  | Rec of name * tp * exp            (* rec (f: t) => e *)
  | Let of name * exp * exp           (* let x = e1 in e2 end *)
  | Apply of exp * (exp list)         (* e (e_1, e_2, ..., e_n) *)
  | Var of name                       (* x *)

(* Some example programs in MiniCAML. *)
(* fun (x: int, y: int) => (x * x) + (y * y) *)
let ex1 : exp =
  Fn ([("x", Int); ("y", Int)],
      Primop (Plus,
              [Primop (Times, [Var "x"; Var "x"]);
               Primop (Times, [Var "y"; Var "y"])]))

(* fun () => true *)
let ex2 : exp = Fn ([], B true)

(* let f = (fun (x: int, y: int) => (x * x) + (y * y))
   in
   f (3, 4)
*)
let ex3 : exp =
  Let ("f", ex1,
       Apply (Var "f", [I 3; I 4]))

(* let g = (fun () => true)
   in
   g () *)
let ex4 : exp =
  Let ("g", ex2,
       Apply (Var "g", []))

(* let f = (fun (x: int, y: int) => (x * x) + (y * y))
   in
   f (3)
   Note: this expression is syntactically valid, but ill-typed!
*)
let ex5 : exp =
  Let ("f", ex1,
       Apply (Var "f", [I 3]))

(* let f = (fun (x: int) => (fun (y: int) => (x * x) + (y * y)))
   in
   (f (3)) (4)
*)
let ex6 : exp =
  Let ("f",
       Fn ([("x", Int)],
           Fn ([("y", Int)],
               Primop (Plus,
                       [Primop (Times, [Var "x"; Var "x"]);
                        Primop (Times, [Var "y"; Var "y"])]))),
       Apply (Apply (Var "f", [I 3]),
              [I 4]))

(* let f = (fun (x: int) => (fun (y: int) => (x * x) + (y * y)))
   in
   f (3, 4)
   Note: this expression is syntactically valid, but ill-typed!
*)
let ex7 : exp =
  Let ("f",
       Fn ([("x", Int)],
           Fn ([("y", Int)],
               Primop (Plus,
                       [Primop (Times, [Var "x"; Var "x"]);
                        Primop (Times, [Var "y"; Var "y"])]))),
       Apply (Var "f", [I 3; I 4]))


(* PART 1: unused_vars *)

(* Deletes every occurence of the elements of xs from l.
   e.g. delete [w; y] [y; x; y; z; w] = [x; z]
*)
let delete (xs : 'a list) (l : 'a list) : 'a list =
  List.filter (fun x -> not (List.mem x xs)) l

(* free_variables e = list of names occurring free in e
   Invariant: every name occurs at most once.

   The algorithm works from the leaves of the expression tree
   upwards. Every time a variable is encountered, it is considered free.
   When a binding construct is encountered (e.g. Let) the declared
   variable is deleted from the set of free variables formed by the union
   of the recursive calls.
   Other constructs simply form the union of the sets of free variables
   from the recursive calls and return it.
*)
let rec free_variables : exp -> name list =
  (* Taking unions of lists.
     If the lists are in fact sets (all elements are unique),
     then the result will also be a set.
  *)
  let union l1 l2 = delete l2 l1 @ l2 in
  let union_fvs es =
    List.fold_left (fun acc exp -> union acc (free_variables exp)) [] es
  in
  function
  | Var y -> [y]
  | I _ | B _ -> []
  | If(e, e1, e2) -> union_fvs [e; e1; e2]
  | Primop (_, args) -> union_fvs args
  | Fn (xs, e) ->
      let xs = List.map fst xs in
      delete xs (free_variables e)
  | Rec (x, _, e) ->
      delete [x] (free_variables e)
  | Let (x, e1, e2) ->
      let e1_vars = free_variables e1 in
      let e2_vars = delete [x] (free_variables e2) in
      union e1_vars e2_vars
  | Apply (e, es) -> union_fvs (e :: es)

(* PART 2: subst *)

(* A substitution [e/x]. This is read as "e for x". *)
type subst = exp * name

(* PART 3: eval *)

(* Runtime errors that may be raised by eval. *)
type runtime_error =
  | Free_variable of name
  | Bad_primop_args
  | If_non_true_false
  | Arity_mismatch
  | Apply_non_fn

exception Stuck of runtime_error

(* Evaluates a primitive operation *)
let eval_op (op : primop) (exps : exp list) : exp option =
  match op, exps with
  | (Equals,   [I i; I i']) -> Some (B (i = i'))
  | (LessThan, [I i; I i']) -> Some (B (i < i'))
  | (Plus,     [I i; I i']) -> Some (I (i + i'))
  | (Minus,    [I i; I i']) -> Some (I (i - i'))
  | (Times,    [I i; I i']) -> Some (I (i * i'))
  | (Negate,   [I i])       -> Some (I (-i))
  | _                       -> None

(* PART 4: infer *)

(* Type contexts *)
type context = (name * tp) list
let empty = []

(* Looks up the topmost x in ctx and returns its corresponding type.
   If the variable x cannot be located in ctx, raises Not_found.
*)
let lookup (x: name) (ctx: context) = List.assoc x ctx

(* Adds a new type ascription to a context. *)
let extend ctx (x, tau) = (x, tau) :: ctx

(* Adds multiple new type ascriptions to a context. *)
let extend_list (ctx: context) (l: (name * tp) list) =
  List.fold_left extend ctx l

(* Type errors that may be raised by infer *)
type type_error =
  | Free_variable of name
  | Apply_non_arrow of tp (* expected an arrow type, but instead found... *)
  | Arity_mismatch
  | Type_mismatch of tp * tp (* (expected type, actual type) *)

exception TypeError of type_error

(* Convenience function for raising type mismatch errors *)
let type_mismatch expected_type inferred_type =
  raise (TypeError (Type_mismatch (expected_type, inferred_type)))

(* Computes the type of a primitive operation.
   The result is a tuple representing the domain and range of the primop.
*)
let primopType (p: primop): tp list * tp = match p with
  | Equals   -> ([Int; Int], Bool)
  | LessThan -> ([Int; Int], Bool)
  | Plus     -> ([Int; Int], Int)
  | Minus    -> ([Int; Int], Int)
  | Times    -> ([Int; Int], Int)
  | Negate   -> ([Int], Int)

(* Part 5: Unification *)

(* We extend types to support variables. We also
   simplify arrow types to be of the form S -> T
   instead of S1 ... Sn -> T *)
type utp =
  | UArrow of utp * utp
  | UInt
  | UBool
  | UTVar of string

module UTVarMap = Map.Make (String)

let rec string_of_utp (subst : utp UTVarMap.t) (tp : utp) : string =
  match tp with
  | UArrow (a, b) -> "(" ^ (string_of_utp subst a) ^ " -> " ^ (string_of_utp subst b) ^ ")"
  | UInt -> "int"
  | UBool -> "bool"
  | UTVar v ->
      match UTVarMap.find_opt v subst with
      | None -> "'" ^ v
      | Some tp -> string_of_utp subst tp

let string_of_substitution (subst : utp UTVarMap.t) : string =
  "{\n"
  ^ (UTVarMap.fold (fun key value acc ->
      acc ^ "  '" ^ key ^ " = " ^ (string_of_utp subst value) ^ ";\n"
    ) subst "")
  ^ "}"

let print_utp tp = print_string @@ string_of_utp UTVarMap.empty tp

let print_substitution subst = print_string @@ string_of_substitution subst

(* Different errors that can arise during unification. *)
type unif_error =
  (* Raised when attempting to unify a type variable 'a with a type t
     of which 'a is a subexpression, e.g. t is UArrow (UInt, 'a) *)
  | UnifOccursCheckFails
  (* Raised when the unifier attempts to unify mismatched types,
     e.g. Bool with Int, or an Arrow with a Bool. *)
  | UnifMismatch of utp (* LHS *) * utp (* RHS *)

(* An exception constructor so that we can raise unif_error values. *)
exception UnifError of unif_error

(* Convenience function for raising unif_error values. *)
let unif_error e = raise (UnifError e)

(* -------------------------------------------------------------*)
(* Other helper functions                                       *)
(* You don't need to look at these to do the assignment, but it *)
(* would be a good idea to understand them.                     *)
(* -------------------------------------------------------------*)

(* Generating fresh (new) variable names *)
type gen_var = {
  fresh: name -> name; (* generates a fresh name based on a given one. *)
  reset : unit -> unit (* resets the internal counter for making names. *)
}

let gen_var : gen_var =
  let counter = ref 0 in
  let fresh x = incr counter; x ^ (string_of_int (!counter)) in
  let reset () = counter := 0 in
  {fresh; reset}

let freshVar = gen_var.fresh
let resetCtr = gen_var.reset

(* Converts a type to a string representation. *)
let rec string_of_tp t = match t with
  | Arrow (t1s, t2) ->
      (String.concat ", " (List.map string_of_tp t1s)) ^
      " -> " ^
      string_of_tp t2
  | Int -> "int"
  | Bool -> "bool"

(* String representations of expressions. Useful for debugging!
   Note that this expression printer is very primitive, but it should suit
   your needs most of the time.
*)
let nl_sep l = String.concat "\n" l

let bracket str = "(" ^ str ^ ")"

let string_of_op p = match p with
  | Equals   -> " = "
  | LessThan -> " < "
  | Plus     -> " + "
  | Minus    -> " - "
  | Times    -> " * "
  | Negate   -> "-"

let rec string_of_exp indent exp =
  let new_ind = indent ^ "  " in
  let string_of_exp' = string_of_exp indent in
  let string_of_exp'' = string_of_exp new_ind in
  match exp with
  | I n ->
      if n < 0 then bracket (string_of_int n)
      else string_of_int n
  | B b -> if b then "True" else "False"
  | If (p, e1, e2) ->
      nl_sep
        ["if " ^ (string_of_exp'' p) ^ " then";
         new_ind ^ (string_of_exp'' e1);
         indent ^ "else";
         new_ind ^ (string_of_exp'' e2)]
  | Primop (p, el) ->
      bracket @@
      if p = Negate then
        (string_of_op p) ^ (string_of_exp' (List.nth el 0))
      else
        (string_of_exp' (List.nth el 0)) ^
        (string_of_op p) ^
        (string_of_exp' (List.nth el 1))
  | Fn (xs, exp) ->
      let params =
        String.concat ", "
          (List.map (fun (x, tp) -> x ^ ": " ^ (string_of_tp tp)) xs)
      in
      bracket @@
      nl_sep
        ["fun (" ^ params ^ ") =>";
         new_ind ^ (string_of_exp'' exp)]
  | Rec (name, tp, exp) ->
      bracket @@
      nl_sep
        ["rec (" ^ name ^ ": " ^ (string_of_tp tp) ^ ") =>";
         new_ind ^ (string_of_exp'' exp)]
  | Let (name, e1, e2) ->
      nl_sep
        ["let " ^ name ^ " = " ^ (string_of_exp' e1) ^ " in";
         new_ind ^ (string_of_exp'' e2)]
  | Apply (e, es) ->
      let params = bracket (String.concat ", " (List.map string_of_exp' es)) in
      (string_of_exp' e) ^ " " ^ params
  | Var name -> name

let print_exp exp = print_string (string_of_exp "" exp)


(*
 Test cases for the unused_vars : exp -> name list function. 
*)
(*
let unused_vars_tests = [ 
  (Let ("x", I 1, I 5), ["x"]); 
  (Var ("x"), []);
  (Let ("x", I 1, I 5), ["x"]);
  (Fn ([("x", Int)], Var ("x")), []);
  (Fn ([("x", Int)], I (1)), ["x"]);
  (Fn ([("x", Int); ("y", Bool)], I (1)), ["x"; "y"]);
  (Rec ("x", Int, I (7)), ["x"]);
  (Rec ("x", Int, Let ("l", I 1, Var ("x"))), ["l"]);
  (Apply (Let ("x", I 1, I 5), [I (4)]), ["x"]);
  (Apply (Let ("x", I 1, I 1), [I (4); Let ("y", I 1, I 1)]), ["x"; "y"]) 
]
*)

(*
 unused_vars : exp -> name list recursively traverses the input expression to
 collect the unused variables. When unused_vars arrives at a binding construct, 
 it decides whether the variable being introduced by the construct is unused by
 checking whether the variable is free in the body of the construct. 
*)
let rec unused_vars =
  function
  | Var _ | I _ | B _ -> []
  | If (e, e1, e2) -> unused_vars e @ unused_vars e1 @ unused_vars e2
  | Primop (_, args) ->
      List.fold_left (fun acc exp -> acc @ unused_vars exp) [] args
  | Let (x, e1, e2) ->
      let unused = unused_vars e1 @ unused_vars e2 in
      if List.mem x (free_variables e2) then
        unused
      else
        x :: unused 
  | Rec (x, _, e) -> 
      let unused = unused_vars e in
      if List.mem x (free_variables e) then 
        unused
      else 
        x :: unused 
  | Fn (xs, e) -> 
      let unused = unused_vars e in
      List.fold_left (fun unused (name, _) -> 
          if List.mem name (free_variables e) then unused
          else name :: unused) 
        unused
        xs 
  | Apply (e, es) -> 
      unused_vars e @ List.fold_left (fun acc exp -> acc @ unused_vars exp) [] es
      
(* 
 Test cases for the subst: exp * name -> exp -> exp function. 
*) 
(*
let subst_tests : (((exp * name) * exp) * exp) list = [ 
  (((I 1, "x"), (* [1/x] *)
    (* let y = 2 in y + x *)
    Let ("y", I 2, Primop (Plus, [Var "y"; Var "x"]))),
   (* let y = 2 in y + 1 *)
   Let ("y", I 2, Primop (Plus, [Var "y"; I 1])));
  
  (((I 9, "x"),
    Fn ([], Let ("y", I 2, Primop (Plus, [Var "y"; Var "x"])))), 
   Fn ([], Let ("y", I 2, Primop (Plus, [Var "y"; I 9]))));
  
  (((Primop (Times, [Var "x"; Var "x"]), "q"),  
    Fn (["u", Int], Let ("y", I 2, Primop (Plus, [Var "q"; Var "y"])))), 
   Fn (["p", Int], 
       Let ("y", I 2, Primop (Plus, [(Primop (Times, [Var "x"; Var "x"])); Var "y"])))); 
  
  (((Primop (Plus, [Var "x"; I 1]), "x"),
    Fn ([("x", Int)], Let ("y", I 2, Var "x"))), 
   Fn([("x1", Int)], Let ("y", I 2, Var "x1")));
  
  (((Primop (Plus, [Var "x"; I 1]), "y"),
    Fn ([("x", Int)], Let ("z", I 2, Var "y"))), 
   Fn ([("x1", Int)], Let ("z", I 2, (Primop (Plus, [Var "x"; I 1])))));
  
  (((I 9, "x"),
    Rec ("z", Int, Let ("y", I 2, Primop (Plus, [Var "y"; Var "x"])))), 
   Rec("z", Int, Let ("y", I 2, Primop (Plus, [Var "y"; I 9]))));
  
  (((Primop (Plus, [Var "x"; I 1]), "x"),
    Rec ("x", Int, Let ("y", I 2, Var "x"))), 
   Rec("x1", Int, Let ("y", I 2, Var "x1")));
  
  (((Primop (Plus, [Var "x"; I 1]), "y"),
    Rec ("x", Int, Let ("z", I 2, Var "y"))), 
   Rec("x1", Int, Let ("z", I 2, (Primop (Plus, [Var "x"; I 1])))));
 
  (((Var "x", "n"),
    Rec ("n", Int, Let ("n", Primop (Plus, [Var "n"; Var "x"]), Primop (Plus, [Var "n"; Var "n"])))), 
   Rec("n", Int, Let ("n", Primop (Plus, [Var "n"; Var "x"]), Primop (Plus, [Var "n"; Var "n"]))));
  
  (((I 6, "y"), 
    Rec ("y", Int, Let ("y", Var "o", Primop (Plus, [Var "n"; Var "x"])))), 
   Rec("y", Int, Let ("y", Var "o", Primop (Plus, [Var "n"; Var "x"]))));
  
  (((I 6, "y"), 
    Rec ("y", Int, I 5)), 
   Rec("y", Int, I 5));
  
  (((I 9, "x"),
    Apply (I 1, [Let ("y", I 2, Primop (Plus, [Var "y"; Var "x"]))])), 
   Apply (I 1, [Let ("y", I 2, Primop (Plus, [Var "y"; I 9]))])); 
  
  (((B true, "x"),
    Apply (
      Var "x", 
      [Let ("y", I 2, Primop (Plus, [Var "y"; Var "x"; Var "x"])); Var "x"]) 
   ), 
   Apply (
     B true, 
     [Let ("y", I 2, Primop (Plus, [Var "y"; B true; B true])); B true])
  ); 
]
*)

(* 
 subst : exp * name -> exp -> exp performs expression substitution:
 subst (e', x) e = [e'/x] e. The first argument (e', x): exp * name 
 represents the substitution that is normally written as [e'/x], where x is the
 variable to be replaced, and e' is the expression to replace it with. The second
 argument, of type exp, is the expression on which to perform the substitution.
*)
let rec subst ((e', x) as s) exp =
  match exp with
  | Var y ->
      if x = y then e'
      else Var y
  | I n -> I n
  | B b -> B b
  | Primop (po, args) -> Primop (po, List.map (subst s) args)
  | If (e, e1, e2) ->
      If (subst s e, subst s e1, subst s e2)
  | Let (y, e1, e2) ->
      let e1' = subst s e1 in
      if y = x then 
        Let (y, e1', e2) 
      else 
        let (y, e2) =
          if List.mem y (free_variables e') then
            rename y e2
          else
            (y, e2)
        in
        Let (y, e1', subst s e2) 
          
  | Rec (y, t, e) -> 
      if y = x then Rec (y, t, e)
      else
        let (y, e) = 
          if List.mem y (free_variables e') then rename y e
          else (y, e)
        in 
        Rec (y, t, subst s e) 
          
  | Fn (xs, e) -> 
      if List.mem x (List.map fst xs)
      then Fn (xs, e) 
      else
        let (xs', e) = 
          let freeVars = free_variables e' in 
          let nms = List.map fst xs in
          if List.exists (fun y -> List.mem y freeVars) nms 
          then 
            let l = List.map fst xs in
            let (renamedParams, newE) = rename_all l e in
            let newList = List.map2 (fun y (_, t) -> (y, t)) renamedParams xs 
            in (newList, newE) 
          else (xs, e) 
        in
        Fn (xs', subst s e) 
          
  | Apply (e, es) -> Apply (subst s e, List.map (subst s) es) 

and rename x e =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and rename_all names exp =
  List.fold_right
    (fun name (names, exp) ->
       let (name', exp') = rename name exp in
       (name' :: names, exp'))
    names
    ([], exp) 
  

(* Applying a list of substitutions to an expression, leftmost first *)
let subst_list subs exp =
  List.fold_left (fun exp sub -> subst sub exp) exp subs

(* 
 Test cases for the eval: exp -> exp function. 
*) 
(*
let eval_tests = [ 
  (Let ("x", I 1, Primop (Plus, [Var "x"; I 5])), I 6);
  (Rec ("x", Int, Let ("x", I 1, Primop (Plus, [Var "x"; I 5]))), I 6); 
  (Apply (ex2, []), B true);
  (Apply (Fn ([], I 1), []), I 1);
  (Apply (ex1, [I 1; I 2]), I 5); 
  (Apply (Fn ([("k", Int)], I 1), [I 6]), I 1); 
  (Apply (Fn ([("x", Int)], Fn ([("k", Int)], I 1)), [I 6]), Fn ([("k", Int)], I 1)); 
  (Apply (Rec ("k", Int, (Fn ([("k", Int)], I 3))), [I 6]), I 3);
]
*)

(*
 eval : exp -> exp behaves like a big-step evaluation interpreter. It takes as 
 input an expression and outputs a value. 
*)
let rec eval exp =
  match exp with
  (* Values evaluate to themselves *)
  | I _ -> exp
  | B _ -> exp
  | Fn _ -> exp

  (* This evaluator is _not_ environment-based. Variables should never
     appear during evaluation since they should be substituted away when
     eliminating binding constructs, e.g. function applications and lets.
     Therefore, if we encounter a variable, we raise an error.
*)
  | Var x -> raise (Stuck (Free_variable x))

  (* Primitive operations: +, -, *, <, = *)
  | Primop (po, args) ->
      let args = List.map eval args in
      begin
        match eval_op po args with
        | None -> raise (Stuck Bad_primop_args)
        | Some v -> v
      end

  | If (e, e1, e2) ->
      begin
        match eval e with
        | B true -> eval e1
        | B false -> eval e2
        | _ -> raise (Stuck If_non_true_false)
      end

  | Let (x, e1, e2) ->
      let e1 = eval e1 in
      eval (subst (e1, x) e2) 

  | Rec (f, _, e) -> eval (subst (exp, f) e)


  | Apply (e, es) -> 
      let e' = eval e in 
      match e' with 
      | Fn (args, expression) -> 
          if List.length args = List.length es then 
            let exp' = 
              subst_list 
                (List.combine (es) (List.map fst args))
                expression
            in
            eval exp'
          else raise (Stuck Arity_mismatch)
      | _ -> raise (Stuck Apply_non_fn)
          
                     

(* 
 Test cases for the infer : context -> exp -> tp function. 
*) 
(*
let infer_tests = [ 
  (([("x", Int)], Var "x"), Int); 
  (([], Rec ("x", Int, Var "x")), Int); 
  (([], Fn ([("x", Int)], Var "x")), Arrow ([Int], Int)); 
  ((["x", Int], Fn ([("x", Int); ("y", Bool)], Var "y")), Arrow ([Int; Bool], Bool)); 
  (([], Fn ([("x", Int); ("y", Bool)], Rec ("x", Int, Var "x"))), Arrow ([Int; Bool], Int)); 
  (([], Fn ([], Rec ("x", Int, Var "x"))), Arrow ([], Int)); 
  (([], Apply (Fn ([("x", Int)], Var "x"), [I 5])), Int);
  (([], Apply (ex1, [I 1; I 2])), Int); 
  (([], Apply (Fn ([], I 7), [])), Int); 
]
*)

(* 
 infer : context -> exp -> tp infers the type of the input expression.
*)
let rec infer ctx e =
  match e with
  | Var x ->
      begin
        try lookup x ctx
        with Not_found -> raise (TypeError (Free_variable x))
      end
  | I _ -> Int
  | B _ -> Bool

  | Primop (po, exps) ->
      let (domain, range) = primopType po in
      check ctx exps domain range

  | If (e, e1, e2) ->
      begin
        match infer ctx e with
        | Bool ->
            let t1 = infer ctx e1 in
            let t2 = infer ctx e2 in
            if t1 = t2 then t1
            else type_mismatch t1 t2
        | t -> type_mismatch Bool t
      end

  | Let (x, e1, e2) ->
      let t1 = infer ctx e1 in
      infer (extend ctx (x, t1)) e2

  | Rec (f, t, e) -> 
      let t' = infer (extend ctx (f, t)) e in 
      if t = t' then t'
      else type_mismatch t t'

  | Fn (xs, e) -> 
      let types = List.map snd xs in
      let tp  = Arrow (types, infer (extend_list ctx xs) e) in tp 
        
  | Apply (e, es) -> 
      let eType = infer ctx e in 
      match eType with
      | Arrow (argType, returnType) -> check ctx es argType returnType 
      | _ -> raise (TypeError (Apply_non_arrow (eType)))
               
and check ctx exps tps result =
  match exps, tps with
  | [], [] -> result
  | e :: es, t :: ts ->
      let t' = infer ctx e in
      if t = t' then check ctx es ts result
      else type_mismatch t t'
  | _ -> raise (TypeError Arity_mismatch)
           
           


(* 
 unify : utp -> utp -> utp UTVarMap.t takes two types t1 and t2 and constructs
 a unifier such that applying it to t1 and t2 produces structurally equal types. 
 If two types are not unifiable, an exception UnifError is raised. 
*)
let unify : utp -> utp -> utp UTVarMap.t =
  let rec unify (substitution : utp UTVarMap.t) (t1 : utp) (t2 : utp) : utp UTVarMap.t =
    match t1, t2 with
    (* Unifying identical concrete types does nothing *)
    | UInt, UInt
    | UBool, UBool -> substitution
    | UTVar a, UTVar a' when a = a' -> substitution

    (* For type constructors, recursively unify the parts *)
    | UArrow (t1, t1'), UArrow (t2, t2') -> unify (unify substitution t1 t2) t1' t2' 
    | UTVar a, _ -> unifyVar substitution a t2
    | _, UTVar b -> unifyVar substitution b t1
    (* All other cases are mismatched types. *)
    | _, _ -> unif_error @@ UnifMismatch (t1, t2)
  
  (* Unify a variable with a type *)
  and unifyVar (substitution : utp UTVarMap.t) (a : string) (t : utp) : utp UTVarMap.t =
    let rec occurs : utp -> bool = function
      | UInt | UBool -> false
      | UArrow (t1, t2) -> occurs t1 || occurs t2
      | UTVar b ->
          if a = b
          then true
          else
            match UTVarMap.find_opt b substitution with
            | None -> false
            | Some t' -> occurs t'
    in 
    if occurs t then unif_error UnifOccursCheckFails 
    else 
      match UTVarMap.find_opt a substitution with
      | Some tp -> unify substitution tp t
      | None -> UTVarMap.add a t substitution 
  in fun t1 t2 -> unify UTVarMap.empty t1 t2


