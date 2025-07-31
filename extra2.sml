
type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

(*1*)
val bedo = {grade = SOME 80, id = 7}

fun pass_or_fail {grade=grd , id=_} =
    case grd of 
         NONE => fail
       | SOME i => 
               if i >= 75 
               then pass
               else fail

(* val w = pass_or_fail bedo *)

(*2*)

fun has_passed {grade=grd, id= i} = 
    let 
        val pof = pass_or_fail {grade=grd , id=i} 
    in 
        case pof of 
             pass => true
           | _ => false
    end

val w = has_passed bedo

(*3*)
(* val students_final_grades_list =  *)
(*     [{grade=SOME 50,id=1}, {grade=SOME 80,id=2},{grade=SOME 100,id=3}] *)

fun number_passed grades = 
    let 
        fun aux (lst , acc ) = 
            case lst of 
                 [] => acc 
               | l::ls => 
                       if has_passed l 
                       then aux (ls, 1 + acc)
                       else aux (ls, acc)
    in 
        aux (grades, 0)
    end

(* val w = number_passed students_final_grades_list *)

(*4*)
(* val a = [(pass, {grade=SOME 50, id=1}), (fail,{grade=SOME 90, id=2}), (pass,{grade=SOME 10, id=3})] *)

fun number_misgraded toto = 
    let
        fun aux ( [] , acc) = acc 
          | aux ( (a, b)::ls , acc) = 
              case (a,has_passed b) of 
                    (pass , false) => aux (ls , 1 + acc)
                  | (fail , true ) => aux (ls , 1 + acc)
                  | (pass , true ) => aux (ls , acc)
                  | (fail , false) => aux (ls , acc)
    in 
        aux (toto,0)
    end

(* val w = number_misgraded a *)

datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me


(*5*)
val family_tree = 
    node {
        value = "khalaf" , 
        left = node {
                    value = "hussein",
                    left = node {
                                value = "mohammed" , 
                                left = leaf,
                                right= leaf
                    }, 
                    right = leaf},
        right = node {
                    value = "karam" , 
                    left = leaf, 
                    right = leaf}}

fun tree_height tr =
    let
        fun max (x,y) = 
            if x > y 
            then x 
            else y 

        fun aux ( leaf, acc ) = acc
          | aux ( node{ left, right, ...}, acc ) =  max (aux (left , 1 + acc) ,aux (right , 1 + acc))
    in 
        aux (tr ,0)
    end

(* val w = tree_height family_tree *)

(*6*)
val my_tree = 
    node { 
        value = 1 ,
        left = node {
                value = 2 , 
                left =  leaf,
                right = leaf},
        right= node {
                value = 3 , 
                left = leaf,
                right = leaf}}

fun sum_tree tr = 
    let 
        fun aux leaf = 0
          | aux ( node {value, left, right}) = value +  aux left + aux right
    in 
        aux tr 
    end

(* val w = sum_tree my_tree *)

(*7*)
val his_tree = 
    node { 
        value = leave_me_alone ,
        left = node {
                value = leave_me_alone, 
                left =  leaf,
                right = leaf},
        right= node {
                value = prune_me , 
                left = leaf,
                right = leaf}}

fun gardener tr = 
    let
        fun aux leaf = leaf
          | aux ( node {value, left,right} ) = 
          case value of 
               prune_me => leaf
             | leave_me_alone => node {value=leave_me_alone, left = aux left,right =  aux right} 
    in 
        aux tr 
    end

(* val w = gardener his_tree *)

(*8*)


datatype nat = ZERO 
             | SUCC of nat

exception Negative

(*9*)             
(* val a = ZERO *)
(* val b = SUCC a *)

fun is_positive ZERO = false
  | is_positive _ = true
     
(* val w = is_positive b *)

(*10*)
(* val a = ZERO *)
(* val b = SUCC a *)
(* val c = SUCC b *)

fun pred ZERO = raise Negative
  | pred (SUCC i) = i

(* val w = pred a *)
(* val w = pred c *)

(*11*)
(* val a = 1 *)
(* val b = 2 *)

fun int_to_nat n = 
    let 
        fun aux (u , acc) = 
            if u = 0 
            then acc
            else aux (u -1 , SUCC acc)
    in 
        if n < 0 
        then raise Negative
        else aux (n, ZERO)
    end

(* val w = int_to_nat a *)
(* val m = int_to_nat b *)

(*12*)
(*
* 1 -> SUCC ZERO
* 2 -> SUCC SUCC ZERO
* 1 + 2 = 3 -> SUCC SUCC ZERO *)
(* val a = SUCC ZERO *)
(* val b = SUCC (SUCC ZERO) *)

fun add (n1,n2) = 
    let
        fun aux (n, u, acc ) = 
            if n = u 
            then acc 
            else aux (n, SUCC u , 1 + acc )

        fun hel2 (m, l) = 
            if l = 0 
            then m 
            else hel2 (SUCC m , l - 1)
    in 
        hel2 (n2 ,aux (n1 , ZERO , 0))
    end

(* val w = add (a,b) *)

(*13*)
(* val a = SUCC (SUCC ZERO) *)
(* val b = SUCC ZERO *)

fun sub (n1, n2) = 
    let         
        (*remove one SUCC*)
        fun pred ZERO = raise Negative
          | pred (SUCC i) = i

        (*how many SUCC are there ?*)
        fun aux (n, u, acc ) = 
            if n = u 
            then acc 
            else aux (n, SUCC u , 1 + acc )

        (*remove one SUCC l time from m*)
        fun hel2 (m, l) = 
            if l = 0 
            then m 
            else hel2 (pred m , l - 1)
    in 
        hel2 (n1, aux (n2, ZERO, 0))
    end

(* val w = sub(a,b) *)

(*14*)
(* val a = SUCC (SUCC ZERO) *)
(* val b = SUCC (SUCC ZERO) *)

fun mult (n1,n2) = 
    let 
        fun is_positive ZERO = false
          | is_positive _ = true

        (*how many SUCC are there ?*)
        fun aux (n, u, acc ) = 
            if n = u 
            then acc 
            else aux (n, SUCC u , 1 + acc )

        (* add m to itself (m+m) for l times*)
        fun hel3 (m,l) = 
            if l = 0 
            then ZERO
            else  (add(m, hel3 (m, l-1)) )
    in 
        if is_positive n1 andalso is_positive n2 
        then hel3(n1 , aux (n2 , ZERO, 0))
        else ZERO
    end

(* val w = mult (a,b) *)

(*15*)
(* val a = SUCC ZERO *)
(* val b = SUCC (SUCC ZERO) *)

fun less_than (n1, n2) = 
    let 
        (*how many SUCC are there ?*)
        fun aux (n, u, acc ) = 
            if n = u 
            then acc 
            else aux (n, SUCC u , 1 + acc )
    in 
        aux (n1, ZERO, 0) < aux (n2, ZERO, 0)
    end

(* val w = less_than(a,b) *)

datatype intSet = 
  Elems of int list (*list of integers, possibly with duplicates to be ignored*)
| Range of { from : int, to : int }  (* integers from one number to another *)
| Union of intSet * intSet (* union of the two sets *)
| Intersection of intSet * intSet (* intersection of the two sets *)
