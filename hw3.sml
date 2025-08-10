exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps 
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

infix |> 
fun x |> f = (f x)

(**** for the challenge problem only ****)

(* datatype typ = Anything *)
(* 	     | UnitT *)
(* 	     | IntT *)
(* 	     | TupleT of typ list *)
(* 	     | Datatype of string *)

(**** you can put all your code here ****)

(*1*)

val hel1 = (fn x => Char.isUpper (String.sub (x,0)))

fun only_capitals xs = List.filter hel1 xs

(* val test1 = only_capitals ["A","B","C"] = ["A","B","C"] *)

(*2*)

fun longest_string1_helper f xs = 
    case xs of 
         [] => "" 
       | _ => foldl f "" xs

val longest_string1 = longest_string1_helper (fn (x,y) => if String.size x > String.size y then x else y )

(* val test2 = longest_string1 ["A","bc", "Z"] = "bc" *)

(*3*)


fun longest_string2_helper f xs = 
    case xs of 
	 [] => "" 
       | z::[] => z
       | _::xs' => foldl f "" xs'

val longest_string2  = longest_string2_helper (fn (x,y) => if  String.size x < String.size y then y else x)

(* val test3 = longest_string2  *)
(* ["#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"]= *)
(* "#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" *)
(*4*)

fun longest_string_helper f xs = 
    if f (2,1)
    then 
	longest_string1 xs
    else 
	longest_string2 xs


val longest_string3 = longest_string_helper (fn (x,y) => x > y) 

val longest_string4 = longest_string_helper (fn (x,y) => x < y)

(* longest_string3/4: Tried to get the longest string from: ["#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~], should have gotten: "#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~(strict inequality) or "#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~(weak inequality) but got something else. [incorrect answer] *)

(* val test4a = longest_string3 *)
(* ["#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"] *)
(* = "#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" *)
(**)
(* val test4b = longest_string4 *)
(* ["#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"] *)
(* = "#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" *)

(*5*)

fun longest_capitalized xs = xs |> only_capitals |> longest_string3

(* val test5 = longest_capitalized ["A","bc","C"] = "A" *)

(*6*)

fun rev_string x = x |> String.explode |> List.rev |> String.implode

(* val test6 = rev_string "abc" = "cba" *)

(*7*)

fun first_answer f [] = raise NoAnswer
       | first_answer f (x::xs) = 
       case f x of 
	    SOME v => v 
	  | NONE => first_answer f xs

(* val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4 *)


(*8*)

fun all_answers f xs = 
    let 
	fun aux [] acc = SOME acc
	  | aux (x::xs') acc = 
	  case f x of 
	NONE => NONE 
      | SOME lst => aux xs' (lst@acc)
    in 
	aux xs []
    end

(* val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE *)

(*9*)

(*a*)
val count_wildcards = g (fn _ => 1) (fn _ => 0) 

val test9a = count_wildcards Wildcard = 1

(*b*)
val count_wild_and_variable_lengths = g (fn _ => 1)  String.size 

val test9b = count_wild_and_variable_lengths (Variable "a") = 1

(*c*)
val conut_some_va = g (fn _ => 0 ) 
