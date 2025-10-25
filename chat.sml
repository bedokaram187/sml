
(*question*)

(*rules*)
(*held cards * goal --> score *)
(*sum = sum of the values of the held cards*)
(*if sum > goal then preliminary score = 3 (sum - goal) else preliminary score = (goal - sum) *)
(*if all the held cards are same color*)
(*then score = preliminary score / 2 *)
(*else score = preliminary score*)

(*output*)
(* score: Your function returns an incorrect result when you must round the score correctly. [incorrect answer] *)
(* score: Your function returns an incorrect result when the input hand is the empty list. [incorrect answer] *)

datatype suit = Clubs 
              | Diamonds 
              | Hearts 
              | Spades
datatype rank = Jack 
              | Queen 
              | King 
              | Ace 
              | Num of int 

type card = suit * rank

datatype color = Red 
               | Black

datatype move = Discard of card 
              | Draw 

exception IllegalMove

val card1 = (Clubs, Num 2)
val card2 = (Spades, Jack)
val card3 = (Diamonds, Ace)
val card4 = (Hearts,Queen )

val my_cards = [card1,card2,card3,card4]
val my_goal = 50

fun score (held_cards, goal) =  
    let 
        fun card_value kard = 
            case kard of 
                 (_,Num i) => i 
               | (_,Ace) => 11
               | (_,_) => 10

        fun card_color c = 
            case c of 
                 (Spades,_) => Black
               | (Clubs,_) => Black
               | (Diamonds,_) => Red
               | (Hearts,_) => Red

        fun all_same_color cards  = 
            case cards of 
                 [] => false (*false*)
               | _::[] => true
               | x::(y::xs') => card_color x = card_color y andalso all_same_color ( y ::xs' )

        fun sum cardz = 
            let 
                fun aux (lst , acc ) = 
                    case lst of 
                         [] => acc  (*goal ??*)
                       | start::finish => aux (finish , card_value start + acc)
            in 
                aux (cardz , 0) 
            end 

        fun pscore (som,gol) =  
            if som > gol
            then 3 * (som - gol) 
            else gol - som

    in 
        if all_same_color held_cards 
        then pscore(sum held_cards, goal ) div 2 
        else pscore (sum held_cards, goal )
    end 

val w = score(my_cards,my_goal)
