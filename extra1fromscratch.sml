(**)
(*q1*)
(* fun alternate (xs: int list) = *)
(* let *)
(*     fun helper ([], sign: int) = 0 *)
(*       | helper (x::xs, sign: int) = (sign * x) + helper(xs, ~sign) *)
(* in *)
(*     helper(xs, 1) *)
(* end *)

(*q2*)
(* fun min_max (l : int list) =  *)
(* let  *)
(**)
(* fun max xs =  *)
(*     if null xs  *)
(*     then NONE   *)
(*     else  *)
(*         let  *)
(*             val tl_ans = max (tl xs ) *)
(*         in  *)
(*             if isSome tl_ans andalso valOf tl_ans > hd xs  *)
(*             then tl_ans  *)
(*             else SOME (hd xs) *)
(*         end *)
(**)
(* fun min xs =  *)
(*     if null xs  *)
(*     then NONE   *)
(*     else  *)
(*         let  *)
(*             val tl_ans = min (tl xs ) *)
(*         in  *)
(*             if isSome tl_ans andalso valOf tl_ans < hd xs  *)
(*             then tl_ans  *)
(*             else SOME (hd xs) *)
(*         end *)
(* in  *)
(*     (valOf(min l) , valOf(max l)) *)
(* end *)
(**)
(* val s = min_max([1,2,3]) = (1,3) *)

(*q3*)
(* fun cumsum (xs: int list) = *)
(* let *)
(*     fun helper (xs, acc) =  *)
(*         case xs of  *)
(*              [] => [] *)
(*            | x::xs' => (x+acc) :: helper (xs', x + acc ) *)
(* in *)
(*     helper(xs, 0) *)
(* end *)
(* val s = cumsum [1,2,3,4] *)


(*q4*)
(* fun greeting n  =  *)
(*     if isSome n *)
(*     then "hello" ^ " " ^ valOf n *)
(*     else "hello you" *)
(**)
(* val s = greeting (SOME "bedo") *)

(*q5*)
(* fun repeat (xs, ys) =  *)
(* let  *)
(*     fun hel1 (m, g)  =  *)
(*         if g = 0  *)
(*         then []  *)
(*         else m :: hel1 (m, g - 1) *)
(* in  *)
(*     if null xs  *)
(*     then []  *)
(*     else hel1 (hd xs , hd ys ) @ repeat (tl xs, tl ys)  *)
(* end *)
(**)
(* val s = repeat ([1,2,3] , [2,2,2]) *)


(*q6*)
(* fun addOpt (x,y) =  *)
(*     if isSome x andalso isSome y  *)
(*     then SOME (valOf x + valOf y ) *)
(*     else NONE *)
(**)
(* val s = addOpt(SOME 5 , NONE) *)

(*q7*)
fun addallOpt xs = 
let 
    fun all_none zs =
        if null zs 
        then true 
        else not (isSome(hd zs)) andalso all_none(tl zs)

    fun hel1 (ys: int option list , acc: int) = 
    if null ys orelse all_none ys
    then NONE
    else if null (tl ys)
    then SOME (valOf(hd ys) + acc)
    else
        if not (isSome(hd ys))
        then hel1(tl ys, acc)
        else hel1(tl ys, valOf(hd ys) + acc)
in 
    hel1 (xs, 0)
end

val s = addallOpt [SOME 5,NONE, SOME 10]
