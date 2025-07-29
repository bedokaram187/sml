
(*5*)
(* val a = [1,2,3]  *)
(* val b = [3,0,1] *)
(* fun repeat (xs, ys) =  *)
(*   let  *)
(*     fun hel1 (x,y) =  *)
(*       if y > 0  *)
(*       then x :: hel1(x,y-1) *)
(*       else [] *)
(*   in *)
(*     case xs of  *)
(*          [] => [] *)
(*        | x::xs' =>  *)
(*            case ys of  *)
(*                 [] => [] *)
(*               | y::ys' => hel1(x,y) @ repeat(xs',ys') *)
(*   end *)
(**)
(* val w = repeat(a,b) *)

(*6*)
(* val a = SOME 1  *)
(* val b = SOME 2  *)
(* fun addOpt (x,y) =  *)
(*   case (x,y) of  *)
(*        (SOME i , SOME e ) => SOME (i + e)  *)
(*      | _ => NONE *)
(**)
(* val w = addOpt(a,b) *)
(**)
(* (*7*) *)
(* val a = [SOME 1 , NONE , SOME 2] *)
(**)
(* fun addAllOpt xs =  *)
(*   case xs of  *)
(*        [] => NONE  *)
(*      | x::xs' =>  *)
(**)
(**)
(* val w = addAllOpt a *)

exception IllegalMove

val card_list = [1,2,3]
val held_list = [4,5,6]

fun draw (xs, ys, ex) = 
  case ys of 
       [] => score (held_cards, my_goal)
     | y::ys' => 
         if isScore(soce(held_cards, my_goal))
         then score(held_cards, my_goal)
         else officete (ys' , xs' ,my_goal )
         y :: xs 

val w = draw(held_list,card_list, ex)

