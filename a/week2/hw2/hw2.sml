
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (str, lst) =
   case lst of
        [] => NONE
      | l::l' => if same_string (l,str) then SOME l' 
                 else case all_except_option (str,  l') of
                           SOME a => SOME ( l::a)
                         | _ => NONE

fun get_substitutions1 (lst, str) = 
   case lst of
        [] => []
      | l::l' => case all_except_option(str,l) of
                  SOME (s::s') => (s::s') @ get_substitutions1 (l', str)    
                 | _ => get_substitutions1 (l', str)

fun get_substitutions2 (lst, str) = 
   let fun aux (lt,str,sub) =
      case lt of
           [] => sub
         | l::l' => case all_except_option(str,l) of
                         SOME (a::a') => aux (l',str, sub @ (a::a'))
                       | _ => aux (l', str, sub)
   in
      aux (lst,str,[])
   end

fun similar_names (string_list_list, full_name) =
   let val {first=a, middle=b,last=c} = full_name  
      val subs = get_substitutions2 (string_list_list, a) 
      fun aux (subs_list, names_list)  =  
         case subs_list of
            [] => names_list 
          | l::l' => aux (l', names_list @ [{first=l,middle=b,last=c}] ) 
   in
      aux (subs, [full_name])
   end

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color card_ =
   case card_ of
        (Spades,_) => Black
      | (Clubs,_) => Black
      | (_) => Red

fun card_color2 card_ =
   case card_ of
        ((Spades,_) | (Clubs,_)) => Black
      | (_) => Red

fun card_value card_ =
   case card_ of
        (_,Num i) => i
      | (_,Ace) => 11
      | (_) => 10

fun remove_card (cs, c, e) =
   case cs of
        [] => raise e
      | l::l' => if l = c then l'
                 else l::remove_card(l',c,e)

fun all_same_color card_list =
   case card_list of
        l1::l2::l' => card_color(l1) = card_color(l2) andalso all_same_color(l2::l')
      | l3::[] => true
      | [] => true

fun sum_cards card_list =
   let fun aux (c_list, count) =
      case c_list of
           [] => count
         | l::l' => aux (l', count + card_value l)
   in
      aux(card_list,0)
   end

fun score (card_list,goal) =
   let val sum = sum_cards card_list
       val goal_variance = if (sum > goal) then 3 * (sum - goal)
                           else goal - sum
   in
      if (all_same_color card_list) then goal_variance div 2
      else goal_variance
   end

fun officiate (card_list, move_list, goal) = 
   let fun aux (moves,hand,deck) =
      let val cur_score = score(hand,goal)
      in case (moves,hand,deck) of
           ([],_,_) => cur_score
         | ((Draw)::moves,_,[]) => cur_score
         | ((Draw)::moves,_,deck_card::deck) => if (cur_score > goal) 
               then cur_score 
               else aux(moves,deck_card::hand,deck)
         | ((Discard c)::moves,hand,deck) =>
               aux(moves,remove_card(hand,c,IllegalMove),deck)
      end
   in
      aux(move_list,[],card_list)
   end

fun count_aces card_list =
   case card_list of
        [] => 0 
      | (suit,rank)::l' => if card_value (suit,rank) = 11
                           then 1 + count_aces(l')
                           else 0 + count_aces(l')

fun score_challenge (card_list, goal) = let
   fun aux (count, min) = let
      val this_min = score(card_list, goal +  10 * count)
   in
      if count = 0 then min
      else if min < this_min
      then aux (count - 1, min)
      else aux (count - 1, this_min)
   end
in
   aux (count_aces(card_list),score(card_list, goal)) 
end

fun officiate_challenge (card_list, move_list, goal) = let
   fun aux_count_aces (card_list,sum,goal) =
      case card_list of
           [] => 0 
         | l::l' => if sum > goal then 0 
                    else if card_value(l) = 11 then 1 + aux_count_aces(l',sum + 1,goal)
                    else aux_count_aces(l',sum + card_value(l),goal)

   fun aux1 (count, min) = 
      let
         val new_min = officiate (card_list, move_list, goal + 10 * count)
      in
         if count = 0 then min
         else if min < new_min
         then aux1(count - 1, min)
         else aux1(count - 1, new_min)
      end
in
   aux1(aux_count_aces(card_list,0,goal),officiate(card_list,[],goal))
end 

fun careful_player (card_list, goal) = let
   fun get_card_matching_value (card_list, value) =
      case card_list of
          [] => NONE
         | l::l' => if card_value(l) = value then SOME l
                    else get_card_matching_value(l',value)

   fun aux (deck, sum, move_list, hand) = let
      val cur_score = (hand,goal)
   in
      case (deck, sum, move_list, hand) of
          ([],_,moves,_) => moves
         | (deck_card::deck,sum,moves,hand) => 
              case get_card_matching_value(hand,sum + card_value(deck_card) - goal) of
                   NONE => if card_value(deck_card) + sum <= goal then aux(deck,sum + card_value(deck_card),moves @ [Draw],deck_card::hand)
                           else moves 
                 | SOME c => moves @ [Discard c] @ [Draw]
   end
in
   aux(card_list, 0, [],[])
end