(*1*)

fun is_older ((y1,m1,d1),(y2,m2,d2)) = 
    y1 < y2 orelse
    (y1 = y2 andalso m1 < m2) orelse
    (y1 = y2 andalso m1 = m2 andalso d1 < d2)

(* val test1 = is_older ((1,2,3),(2,3,4)) = true *)

(*2*)

fun number_in_month (xs,n) =
    let 
        fun aux (ys , acc) =
            case ys of 
                 [] => acc
               | (_,s,_)::ys' => 
                       if s = n 
                       then aux (ys', 1 + acc ) 
                       else aux (ys', acc)
    in 
        aux (xs,0)
    end

(* val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1 *)

(*3*)

fun number_in_months (xs,ns ) = 
    case ns of 
         [] => 0 
       | n::ns' => number_in_month(xs,n) + number_in_months(xs,ns')

(* val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3 *)

(*4*)

fun dates_in_month (ds,n) = 
    case ds of 
         [] => [] 
       | (r,s,t)::ds' => 
               if s = n 
               then (r,s,t) :: dates_in_month(ds',n)
               else dates_in_month(ds',n)

(* val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)] *)

(*5*)

fun dates_in_months (ds,ns) = 
    case ns of 
         [] => []
       | n::ns' => dates_in_month(ds,n) @ dates_in_months(ds,ns')

(* val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)] *)

(*6*)

fun get_nth (st,n) = 
    case st of 
         [] => ""
       | s::st' => 
               if n = 1 
               then s 
               else get_nth(st',n - 1 )

(* val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there" *)

(*7*)
val months = ["January", "February", "March", "April", "May", "June", "July",
"August", "September", "October", "November", "December"]

fun date_to_string (r,s,t) = 
     get_nth(months,s) ^ " " ^ Int.toString t ^ ", " ^ Int.toString r

(* val test7 = date_to_string (2013, 6, 1) = "June 1, 2013" *)

(*8*)

fun number_before_reaching_sum (sum, lst) =
    let
        fun aux (lista , acc , count) = 
            case lista of
                 [] => count 
               | x::xs => 
                       if acc + x >= sum 
                       then count 
                       else aux (xs, acc + x , count + 1)
    in
        aux (lst, 0, 0)
    end

(* val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3 *)

(*9*)
val months_days = [0,31,28,31,30,31,30,31,31,30,31,30]

fun what_month n = 
    number_before_reaching_sum(n, months_days)

(* val test9 = what_month 70 = 3 *)

(*10*)

fun month_range (d1,d2) = 
    if d1 > d2 then [] 
    else what_month d1 :: month_range(d1 + 1 , d2)

(* val test10 = month_range (31, 34) = [1,2,2,2] *)

(*11*)

fun oldest date = 
    let 
        fun is_older ((y1,m1,d1),(y2,m2,d2)) = 
            y1 < y2 orelse
            (y1 = y2 andalso m1 < m2) orelse
            (y1 = y2 andalso m1 = m2 andalso d1 < d2)

        fun hel1 (n,lst) = 
            case lst of 
                 [] => n 
               | l::ls' => 
               if is_older(l,n) 
               then hel1 (l,ls') 
               else hel1(n,ls')
    in 
    case date of 
         [] => NONE 
       | x::xs' =>  SOME (hel1 (x,xs'))
    end

(* val test11 = oldest [(2012,2,28),(2011,3,31),(2011,4,28)] = SOME (2011,3,31) *)
