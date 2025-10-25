
fun map (f, xs)= 
    case xs of 
         [] => [] 
       | x::xs' => (f x) :: map(f,xs')

(* fun filter (f,xs) =  *)
(*     case xs of  *)
(*          [] => []  *)
(*        | x::xz =>  *)
(*                if f x  *)
(*                then x::(filter(f,xz)) *)
(*                else filter(f,xz) *)

datatype exp = Constant of int 
             | Negate of exp 
             | Add of exp * exp 
             | Multiply of exp * exp 

val n1 = Constant 4 
val n2 = Negate n1
val n3 = Add (n1,n2) 
val n4 = Multiply (n1,n2)

val my_exps = [n1,n2]

fun is_even v = 
    case v of 
         Constant i  =>  i mod 2 = 0 
       | _ => is_even v

fun filter (f,xs) = 
    case xs of 
         [] => [] 
       | x::xz => 
               if f x 
               then x::(filter(f,xz))
               else filter(f,xz)

val w = filter(is_even, my_exps)

(* fun double_or_triple f =  *)
(*     if f 7  *)
(*     then fn x => 2*x  *)
(*     else fn x => 3*x *)
(**)
(* val double = double_or_triple(fn x => x-3 = 4) 5 *)
(* val nine = double_or_triple(fn x => x + 1 = 9) 3 *)
(**)

