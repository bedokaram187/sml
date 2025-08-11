
(*1*)

(* fun alternate lst =  *)


(*2*)

fun  min_max [] = (0,0)
  | min_max (l::ls) = 
        (foldl (fn (x,y) => if y < x then y else x) l ls, foldl (fn (x,y) => if x >= y then x else y) l ls)

val test2 = min_max [1,2,3] = (1,3)

(*3*)

(* fun cumsum lst = *)
(*     case lst of *)
(*          [] => [] *)
(*        | m::[] => [m] *)
(*        | _::g::ls => *)
(*                (foldl (fn (x,y) => x+y ) g (g::ls))::[] *)

val lst = [1,2,3,4]
(*[1,3,6,10]*)

fun cumsum xs = 
    let 
        fun aux (zs , acc, out) = 
        case zs of 
             [] => out
           | e:: [] => e + acc :: out
           | x::g::xs' => aux (g :: xs' ,x + g + acc ,  acc ::out )
    in 
        hd xs :: aux (xs , 0, [])
    end 

val m = cumsum lst

(* fun cumsum lst =  *)
(*     let *)
(*         fun hel1 (zs,acc,out) *)
(*         foldl (fn (x,y) => (x + acc)::out ) acc zs *)
(*     in  *)
(*         hel1(lst,0,[]) *)
(*     end *)
(*     let *)
