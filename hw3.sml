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

(**** for the challenge problem only ****)

(* datatype typ = Anything *)
(* 	     | UnitT *)
(* 	     | IntT *)
(* 	     | TupleT of typ list *)
(* 	     | Datatype of string *)

(**** you can put all your code here ****)

infix |> (* tells the parser |> is a function that appears between its two arguments *)
fun x |> f = f x

(*1*)
(* val name = ["bedo", "Ahmed", "Karam"] *)

val hel1 = (fn x => Char.isUpper (String.sub (x,0)))

fun only_capitals xs = List.filter hel1 xs

(* val only_capitals = only_capitals name *)

(*2*)
(* val name = ["1234", "bedo", "abdullah"] *)

fun hel2 (z,y) = 
    if String.size z > String.size y 
    then z 
    else y

fun longest_string1 xs = 
    case xs of 
         [] => "" 
       | _ => foldl hel2 "" xs

(* val longest_string2 = longest_string1 name *)

(*3*)
(* val name = ["1234", "bedo", "abdullah"] *)

fun hel2 (z,y) = 
    if String.size z > String.size y 
    then z 
    else y

fun longest_string3 xs = 
    case xs of 
         [] => "" 
       | _::xs' => foldl hel2 "" xs'

(* val longest_string2 = longest_string1 name *)

(*4*)

fun lo (z, y) = 
    if String.size z > String.size y 
    then z 
    else y

(* fun yo (z, y) =  *)
(*     if String.size z < String.size y  *)
(*     then y *)
(*     else z *)

val w = lo ("123", "12")

fun longest_string_helper f xs = 
    if (f ("123", "12") = "123")
    then 
        case xs of 
             [] => ""
           | _ => foldl f "" xs
    else 
        case xs of 
             [] => ""
           | _::xs' => foldl f "" xs'

val longest_string3 = longest_string_helper lo ["bedo", "ahmed", "karam"]

(* val longest_string4 = longest_string_helper yo ["bedo", "ahmed", "karam"] *)

