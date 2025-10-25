
infix |> 
fun x |> f = f x

(*1*)

fun compose_opt f g x  = 
    case g x of 
         NONE => NONE 
       | SOME b => 
               case f b of 
                    NONE => NONE 
                  | SOME a => SOME a

(*2*)

fun do_until f p x = 

