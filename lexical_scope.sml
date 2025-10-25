
val x = 1 

fun f y =
    let 
        val  x = 1  + y 
    in 
        fn z => x + y + z  (*2y + 1+ z*)
    end

val x = 3 (*shadowed x = 1*)

val g = f 4 (*x = 5
               g = 2 * 5 + 1 + z *)  

val y = 5 (*y = 5 shadowed y = 0*)

val z = g 6 (*x = 7 
               z = 2 * 7 + 1 + z *)
fun f g = 
    let 
        val x = 9 
    in 
        g() 
    end

val x = 7 (*irrelevant*)

fun h() = x+1

val y = f h
