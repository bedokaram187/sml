fun same_string(s1 : string, s2 : string) =
    s1 = s2

(*a*) (*pass*)
(* val name = "bedo" *)
(* val family = ["bedo"] *)

(*takes a string and a list of strings. if that string is in that list => return
* SOME of a list of that list but without that string, else returns NONE *)
fun all_except_option (s , sl ) = 
    let 
        (*takes a string and a string list, and returns that list without that
        * string Provided*)
        fun remove_item (m,xs) = 
           case xs of 
                [] => [] 
              | x::xs' =>
                    if same_string(m,x) 
                    then remove_item (m,xs')
                    else x :: remove_item (m, xs')
        fun exists (h , ws ) = 
            case ws of 
                [] => false 
               | w::ws' => 
                   if same_string(h,w)
                   then same_string(h,w)
                   else exists(h,ws')
        fun some_or_none (n, ys) =
            if exists (n,ys) 
            then SOME (remove_item(n, ys))
            else NONE
    in 
        some_or_none (s,sl)
    end 

(* val z = all_except_option(name,family) *)

(*b*) (*pass*)
(*takes a string and a string list list, and returns a string list that has
* strings that are with in the same list with that string, but the final list
* doesn't have the Provided string *)
(* val name = "bedo" *)
(* val family = [ ["bedo" ], ["ahmed","karam" , "khalaf"] ] *)
(**)
fun get_substitutions1 ( sll,s ) = 
    case sll of 
         [] => []
       | x::xs' =>
               case all_except_option(s,x) of 
               NONE => get_substitutions1 (xs', s)
              | SOME i  => i @ get_substitutions1 ( xs',s) 
(**)
(* val b = get_substitutions1 (family,name) *)
(**)
(*c*) (*pass*)
(* val name = "bedo" *)
(* val family = [ ["bedo" ] , ["ahmed","karam" , "khalaf"] ] *)

fun get_substitutions2 ( sll,s) = 
    let 
        fun aux (xs, acc) = 
            case xs of 
                [] => acc 
                | x::xs' => 
                    case all_except_option(s,x) of 
                         NONE => aux (xs' , acc)
                       | SOME i => aux(xs', acc @ i)
    in 
        aux (sll , [])
    end
(* val s =get_substitutions2 (family,name) *)

(*d*) (*pass*)
(* val name = {first= "Fred" , middle= "W", last= "Smith"} *)
(* val family = [ ["Fred" , "Fredrick"], ["Elizabeth","Betty"] , ["Freddie", "Fred","F"] ] *)

fun similar_names (sll,name) = 
    let
        fun hel1 {first=n , middle=_ , last=_} = 
            get_substitutions2 (sll,n)
        fun hel2  (s , {first=_ , middle=m , last=l} ) = 
            {first=s,middle=m,last=l}
        fun hel3 ys = 
            case ys of 
                 [] => [] 
               | x::xs' =>   hel2 (x , name) :: hel3 xs' 
    in 
        name :: hel3(hel1 name)
    end 

(* val w = similar_names(family ,name) *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
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

(* put your solutions for problem 2 here *)

(*a*)(*pass*)
(* val card1 = (Clubs, Num 2) *)

fun card_color c = 
    case c of 
         (Spades,_) => Black
       | (Clubs,_) => Black
       | (Diamonds,_) => Red
       | (Hearts,_) => Red

(* val w = card_color card1 *)

(*b*) (*pass*)
(* val card2 = (Spades, Ace) *)

fun card_value kard = 
    case kard of 
         (_,Num i) => i 
       | (_,Ace) => 11
       | (_,_) => 10

(* val w = card_value card2 *)

(*c*)(*pass*)
(* val card1 = (Clubs, Num 2) *)
(* val card2 = (Diamonds, Ace) *)
(* val card3 = (Hearts,Queen ) *)
(* val card4 = (Spades, Jack) *)

(* val my_cards = [card1,card2,card1,card3,card4] *)

fun remove_card (cs ,c, e) = 
    let 

        fun exist_times (b , bs) = 
            case bs of 
                 [] => 0
                | x::xs => 
                        if b = x 
                        then 1 + exist_times(b , xs)
                        else exist_times(b,xs)

        fun remove_once (l , ls) = 
            case ls of 
                 [] => []
               | u::us => 
                       if u = l 
                       then remove_once(l , us) 
                       else u :: remove_once(l , us)
        fun remove_first (p , ps) = 
            case ps of 
                 [] => [] 
               | j::js => 
                       if j = p 
                       then js 
                       else j :: remove_first(p , js)
    in 
        if exist_times (c , cs ) = 1 
        then remove_once (c ,cs ) 
        else if exist_times (c, cs ) > 1 
        then remove_first(c ,cs )
        else raise e
    end
(**)
(* val w = remove_card(my_cards,card1,IllegalMove) *)

(*d*)
(* val card1 = (Clubs, Num 2) *)
(* val card2 = (Spades, Jack) *)
(* val card3 = (Diamonds, Ace) *)
(* val card4 = (Hearts,Queen ) *)
(**)
(* val my_cards = [card1,card2] *)
(* val his_cards = [] *)
(* (**) *)
fun all_same_color cards  = 
    case cards of 
         [] => true (*false*)
       | _::[] => true
       | x::(y::xs') => card_color x = card_color y andalso all_same_color ( y ::xs' )
(**)
(* val w = all_same_color my_cards *)

(*e*)(*pass*)
(* val card1 = (Clubs, Num 2) *)
(* val card2 = (Diamonds, Ace) *)
(* val card3 = (Hearts,Queen ) *)
(* val card4 = (Spades, Jack) *)
(**)
(* val my_cards = [card1,card2,card3,card4] *)
(**)
fun sum_cards cards = 
    let 
        fun card_value kard = 
            case kard of 
                 (_,Num i) => i 
               | (_,Ace) => 11
               | (_,_) => 10

        fun aux (lst , acc ) = 
            case lst of 
                 [] => acc 
               | x::xs => aux (xs , card_value x + acc)
    in 
        aux (cards , 0) 
    end 
(**)
(* val z = sum_cards my_cards *)

(* score: Your function returns an incorrect result when the sum is greater than the goal, and the hand contains cards of oly one color. [incorrect answer] *)
(* score: Your function returns an incorrect result when the sum is not greater than the goal, and the hand contains cards hand of both colors. [incorrect answer] *)
(* score: Your function returns an incorrect result when you must round the score correctly. [incorrect answer] *)
(* score: Your function returns an incorrect result when the input hand is the empty list. [incorrect answer] *)
(*f*)(*pass*)
(*rules*)
(*held cards * goal --> score *)
(*sum = sum of the values of the held cards*)
(*if sum > goal then preliminary score = 3 (sum - goal) else preliminary score = (goal - sum) *)
(*if all the held cards are same color*)
(*then score = preliminary score / 2 *)
(*else score = preliminary score*)

(* val card1 = (Clubs, Num 2) *)
(* val card2 = (Spades, Jack) *)
(* val card3 = (Diamonds, Ace) *)
(* val card4 = (Hearts,Queen ) *)
(**)
(* val my_cards = [card1,card2,card3,card4] *)
(* val my_goal = 50 *)

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
        if held_cards = [] 
        then goal
        else 
            if all_same_color held_cards 
            then pscore(sum held_cards, goal ) div 2 
            else pscore (sum held_cards, goal )
    end 

(* val w = score(my_cards,my_goal) *)

(*g*)
(*rules*)
(*drawing -> removing the first card in the card list and putting it in the
* held cards*)
(*discarding -> choosing one of the held cards to remove *) (*how to choose? you
mean randon, or the first one ?*)
(*the game ends when : 
* - the players chooses to make no more moves (the move list is empty) (*check*)
* - the sum of the value of the held cards is greater than the goal*) (*sum of
                         the vale pscore or the final score?*) (*check?*)
(* the objective is to end the game with low score or zero at best*)
(*if the player discards card c and it's not in the held list will give
* exception IllegalMove*) (*check*)
(* it the player draws and the card list is empty 
* then GAME OVER (*check*) 
* else if drawing means the sum of the values of the held cards exceeds the goal 
* then GAME OVER
* else play with a bigger held cards list and smaller card list*)

(* val card1 = (Clubs, Num 2) *)
(* val card2 = (Diamonds, Ace) *)
(* val card3 = (Hearts,Queen ) *)
(* val card4 = (Spades, Jack) *)
(**)
(* val mov1 = Draw *)
(* val mov2 = Discard card4 *)
(* val mov3 = Discard card2  *)
(* val mov4 = Draw  *)
(**)
(* val cards_list = [card1,card2,card3,card4] *)
(* val my_moves = [mov1,mov2,mov3,mov4] *)
(* val my_goal = 50 *)
(**)
(* fun officiate (held_list ,card_list , move_list , the_goal ) =  *)
(*    let  *)
(*         fun remove_card (c ,cs , e) =  *)
(*             let  *)
(*                 fun exist_times (b , bs) =  *)
(*                     case bs of  *)
(*                          [] => 0 *)
(*                         | x::xs =>  *)
(*                                 if b = x  *)
(*                                 then 1 + exist_times(b , xs) *)
(*                                 else exist_times(b,xs) *)
(*                 fun remove_once (l , ls) =  *)
(*                     case ls of  *)
(*                          [] => [] *)
(*                        | u::us =>  *)
(*                                if u = l  *)
(*                                then remove_once(l , us)  *)
(*                                else u :: remove_once(l , us) *)
(*                 fun remove_first (p , ps) =  *)
(*                     case ps of  *)
(*                          [] => []  *)
(*                        | j::js =>  *)
(*                                if j = p  *)
(*                                then js  *)
(*                                else j :: remove_first(p , js) *)
(*             in  *)
(*                 if exist_times (c , cs ) = 1  *)
(*                 then remove_once (c ,cs )  *)
(*                 else if exist_times (c, cs ) > 1  *)
(*                 then remove_first(c ,cs ) *)
(*                 else raise e *)
(*             end  *)
(*         fun score (held_cards, goal) =   *)
(*             let  *)
(*                 fun card_value kard =  *)
(*                     case kard of  *)
(*                          (_,Num i) => i  *)
(*                        | (_,Ace) => 11 *)
(*                        | (_,_) => 10 *)
(**)
(*                 fun all_same_color cards  =  *)
(*                     case cards of  *)
(*                          [] => false *)
(*                        | _::[] => true *)
(*                        | head::(neck::tail) => head = neck andalso all_same_color ( neck ::tail ) *)
(**)
(*                 fun sum cardz =  *)
(*                     let  *)
(*                         fun aux (lst , acc ) =  *)
(*                             case lst of  *)
(*                                  [] => acc  *)
(*                                | start::finish => aux (finish , card_value start + acc) *)
(*                     in  *)
(*                         aux (cardz , 0)  *)
(*                     end  *)
(**)
(*                 fun pscore (som,gol) =   *)
(*                     if som > gol *)
(*                     then 3 * (som - gol)  *)
(*                     else gol - som *)
(*             in  *)
(*                 if all_same_color held_cards  *)
(*                 then pscore(sum held_cards, goal ) div 2  *)
(*                 else pscore (sum held_cards, goal ) *)
(*             end  *)
(*        fun isScore (i,g) =  *)
(*            i < g *)
(*    in  *)
(*        if isScore(score(held_list,the_goal ),the_goal)  *)
(*        then  *)
(*           case move_list of  *)
(*                [] => score (held_list, the_goal ) *)
(*              | x::xs' =>  *)
(*                    case x of  *)
(*                         Draw => (*remove first card from card list and put it in held list *) *)
(*                                 (*if card list is empty then game over*) *)
(*                                 (*if not then check score and goal, if bigger then *)
(*                                     * GAME OVER*) *)
(*                                 (*else recurse with a smaller card list and a bigger *)
(*                                 * held cards*) *)
(*                                 case card_list of  *)
(*                                      [] => score(held_list,the_goal ) *)
(*                                    | ( l::_) =>  *)
(*                                            officiate((l::held_list),(remove_card(l,card_list,IllegalMove)),xs',the_goal) *)
(*                                            (*draw*) *)
(*                                            (*recurse*) *)
(*                       | Discard i => *)
(*                               officiate(remove_card(i,held_list,IllegalMove),card_list,xs',the_goal) *)
(*       else score(held_list,the_goal ) *)
(*    end  *)
(**)
(* val w = officiate ([],cards_list, my_moves, 50)   *)
