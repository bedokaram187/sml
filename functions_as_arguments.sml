
fun n_times (f, n, x) = 
    if n = 0 
    then x 
    else f (n_times(f, n-1, x)) (*f(f(f(...(f(x)))))*)

fun double x = x+x
val x1 = n_times(double, 5,1)

fun increment x = x + 1 
val x2 = n_times(increment, 5,1)

fun decrement x = x-1 
val x5 = n_times(decrement, 5,10)

val x3 = n_times(tl, 2 , [12,5,6])

fun triple (n,x) = 
    n_times(fn y => 3 * y, n,x) (*anonymos hahaha*)

val x4 = triple(1,3)

(*how many times should i apply function f to x until x reaches zero*)
fun times_untill_zero (f,x) = 
    let
        fun aux (x,acc) =
            if x = 0 
            then acc 
            else aux (f x ,1 + acc)
    in 
        aux (x,0)
    end
(* The map function takes a list and a function f and produces a new list by applying f to each element of the *)
(* list. *)
fun map (f,xs) = 
    case xs of 
         [] => [] 
       | x::xs' => (f x)::map(f,xs')

val y1 = map(increment, [1,2,3]) (*[2,3,4]*)
val y2 = List.map increment [1,2,3](*[2,3,4]*) (*List.map is defined using currying*)

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

fun get_all_even_2nds xs = filter(fn (_,v) => v mod 2 = 0 , xs) (*[(1,2),(3,4),(5,6)]  *)


datatype exp = Constant of int 
             | Negate of exp 
             | Add of exp*exp
             | Multiply of exp*exp 

fun is_even v = 
    (v mod 2 = 0)

fun true_of_all_constant (f,e) = 
    case e of 
         Constant i => f i 
       | Negate e1  => true_of_all_constant(f , e1) 
       | Add (e1,e2) => true_of_all_constant(f,e1) andalso true_of_all_constant(f, e2)
       | Multiply (e1,e2) => true_of_all_constant(f,e1) andalso true_of_all_constant(f, e2)

fun all_even e = true_of_all_constant(is_even, e)

(* val x = 1  *)
(* fun f y = x + y (*1+y*) *)
(* val x = 2 (*ok, x is 2 now*) *)
(* val y = 3 (*ok, y is 3 now *) *)
(* val z = f (x+y) (*(2+3) + 1 = 6*) *)

(* Notice the argument was evaluated in the current environment (producing 5), but the function body was *)
(* evaluated in the “old” environment. where x is 1 *)

(* val x = 1 (*not used*) *)
(* fun f y = *)
(*     let *)
(*         val q = y+1  *)
(*     in *)
(*         fn z => q + y + z *)
(*     end *)
(* val x = 3 (*will not be used *) *)
(* val g = f 4 (*this is using the first function f y -> x = 5, y = 4 *) *)
(* val y = 5 (*this part is confusing *) (*probably will be shadowed*) *)
(* val z = g 6 (*15*) *)
(**)
(* fun f g = *)
(*     let *)
(*         val x = 3 *)
(*     in *)
(*         g 2 *)
(*     end *)
(* val x = 4 (*not used*) *)
(* fun h y = x + y (*4 + y*) *)
(* val z = f h (*5*) *)

fun fold (f,acc, xs) = 
    case xs of 
         [] => acc 
       | x::xs' => fold (f, f (acc,x) , xs')

val z1 = fold(fn (x,y) => x+y , 0 , [1,2,3])

fun numberInRange (xs,lo,hi) = fold ((fn (x,y) => x + (if y >= lo andalso y <= hi then 1 else 0)), 0, xs)
val z2 = numberInRange([1,2,3], 1,3)

fun areAllShorter (xs,s) =
    let
        val i = String.size s
    in
        fold((fn (x,y) => x andalso String.size y < i), true, xs)
    end
