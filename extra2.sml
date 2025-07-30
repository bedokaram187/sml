
type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

(*1*)
val bedo = {grade = SOME 80, id = 7}

fun pass_or_fail {grade=grd , id=_} =
    case grd of 
         NONE => fail
       | SOME i => 
               if i >= 75 
               then pass
               else fail

(* val w = pass_or_fail bedo *)

(*2*)

fun has_passed {grade=grd, id= i} = 
    let 
        val pof = pass_or_fail {grade=grd , id=i} 
    in 
        case pof of 
             pass => true
           | _ => false
    end

val w = has_passed bedo

(*3*)
(* val students_final_grades_list =  *)
(*     [{grade=SOME 50,id=1}, {grade=SOME 80,id=2},{grade=SOME 100,id=3}] *)

fun number_passed grades = 
    let 
        fun aux (lst , acc ) = 
            case lst of 
                 [] => acc 
               | l::ls => 
                       if has_passed l 
                       then aux (ls, 1 + acc)
                       else aux (ls, acc)
    in 
        aux (grades, 0)
    end

(* val w = number_passed students_final_grades_list *)
