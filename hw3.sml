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

fun map (f,xs) = 
    case xs of 
         [] => [] 
       | x::xs' => (f x)::map(f,xs')

fun filter (f,xs) = 
    case xs of 
         [] => []
       | x::xs' => 
               if f x 
               then x::(filter(f,xs'))
               else filter(f,xs')

fun fold (f,acc, xs) = 
    case xs of 
         [] => acc 
       | x::xs' => fold (f, f (acc,x) , xs')
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

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
(* val only_capitals = only_capitals name *)

(*2*)
(* val name = ["1234", "bedo", "abdullah"] *)

fun hel1 (z,y) = 
    if String.size z > String.size y 
    then z 
    else y

fun longest_string1 xs = 
    case xs of 
         [] => "" 
       | _ => foldl hel1 "" xs

val test2 = longest_string1 ["A","bc","C"] = "bc"
(* val longest_string1 = longest_string1 name *)

(*3*)
(* val name = ["1234", "bedo" ] *)

fun hel2 (z,y) = 
    if String.size z > String.size y 
    then z 
    else y

fun longest_string2 xs = 
    case xs of 
         [] => "" 
       | _::xs' => foldl hel2 "" xs'

val test3 = longest_string2 ["A","bc","C"] = "bc"
(* val w = longest_string2 name  *)

(*4*)
val names = ["bedo", "ahmed", "karam"]


fun lo (z, y) = 
    if String.size z > String.size y 
    then z 
    else y 

fun yo (z, y) = 
    if String.size z < String.size y 
    then y
    else z

fun longest_string_helper f xs = 
    if f ("123", "12") = "123"
    then 
	case xs of 
	     [] => "" 
	   | _ => foldl f "" xs
    else 
	case xs of 
	     [] => "" 
	   | _::xs' => foldl f "" xs'

val longest_string3 = longest_string_helper lo 

val longest_string4 = longest_string_helper yo 

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

(*5*)
(* val name = ["bedo", "Ahmed", "karam", "Khalaf"] *)

fun longest_capitalized xs = xs |> only_capitals |> longest_string3

(* val w = longest_capitalized name *)
val test5 = longest_capitalized ["A","bc","C"] = "A"

(*6*)
val name = "bedo" (* "odeb" *)

fun rev_string x = x |> String.explode |> List.rev |> String.implode

(* val w = rev_string name *)
val test6 = rev_string "abc" = "cba"


