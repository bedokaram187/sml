
(*date --> int*int*int*) 
(*year, month, day*)
(*positive, 1-12, 1-31*)
(*day of the year --> 1-365*)

(*q1*)
fun is_older (date1: int*int*int, date2:int*int*int) = 
    (#1 date1 < #1 date2) orelse 
    (#1 date1 = #1 date2 andalso (
    (#2 date1 < #2 date2) orelse 
    (#2 date1 = #2 date2 andalso #3 date1 < #3 date2)
    ))

val test1 = is_older ((1,2,3),(2,3,4)) = true

(*q2*)
val date_list = [(1984,6,7), (2003,7,11), (2001,11,9)]
val my_month = 7

fun number_in_month(dates: (int*int*int) list, month: int) = 
let 
    fun hel1 (xs: int*int*int, n: int) = 
        (#2 xs) = n

    fun hel2 (ys: (int*int*int) list, acc: int) = 
        if null ys 
        then acc
        else 
            if hel1 (hd ys, month) 
            then hel2 (tl ys, acc + 1 )
            else hel2 (tl ys, acc )
in 
    hel2 (dates, 0)
end

val test2 = number_in_month ( [(2012,2,28),(2013,12,1), (2001,11,9)] ,2) = 1

(*q3*)
fun number_in_months(mydates: (int*int*int) list, mymonths: int list) =
let 
    fun hel3 (xs: int list ,acc: int) = 
    if null xs 
    then acc 
    else number_in_month(mydates, hd xs ) + hel3 (tl xs , acc)
in 
    hel3 (mymonths, 0)
end

val test3 = number_in_months ([(2012,2,28),(2013,2,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 4

(*q4*)
fun dates_in_month (dates, month) = 
let 
    fun hel1 (xs: int*int*int, n:int) = (#2 xs) = n 

    fun hel2 (ys, d )= 
        if null ys 
        then d
        else 
            if hel1(hd ys, month) 
            then hel2(tl ys, (hd ys) :: d)
            else hel2(tl ys, d)
in 
    hel2 (dates, [])
end

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

(*q5*)
fun dates_in_months (dates, months) = 
let 
    fun hel4 zs = 
        if null zs 
        then [] (*wrong*)
        else dates_in_month (dates, hd zs) @ hel4 (tl zs)
in 
    hel4 months
end

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

(*q6*)
fun get_nth (malist, n) =
    if n = 1 
    then hd malist 
    else get_nth(tl malist, n - 1)

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

(*q7*)
val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

fun date_to_string (madate: int*int*int) = 
    get_nth(months, #2 madate)  ^ " " ^ Int.toString (#3 madate) ^ ", " ^ 
    Int.toString(#1 madate)

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

(*q8*)
fun number_before_reaching_sum (sum: int, lista:int list )= 
let 
    fun hel5 (xs: int list , acc: int, col:int ) = 
        if  null xs orelse col + hd xs >= sum 
        then acc 
        else hel5 (tl xs , acc + 1, hd xs + col)
in 
    hel5 (lista, 0, 0)
end 


val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

(*q9*)
val months_days = [0,31,28,31,30,31,30,31,31,30,31,30]

fun what_month d = 
    number_before_reaching_sum(d, months_days)

val test9 = what_month 70 = 3

(*q10*)
fun month_range (d1, d2) = 
let 
    fun hel6 (x1, x2) = 
        if x1 > x2 
        then [] 
        else x1 :: hel6(x1 + 1 , x2)

    fun hel7 malist =
        if null malist
        then []
        else what_month(hd malist) :: hel7 (tl malist)
in
    hel7 (hel6(d1,d2))
end

val test10 = month_range (31, 34) = [1,2,2,2]

(*q11*)
fun oldest (dates: (int*int*int) list) = 
let 
    fun hel8 (x, l) = 
        if null l 
        then true
        else is_older (x, hd l) andalso hel8 (x, tl l)
in 
    if null dates 
    then NONE 
    else 
        if hel8 (hd dates, tl dates)
        then SOME (hd dates) 
        else oldest(tl dates)
end
val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
