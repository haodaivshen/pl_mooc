(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1a fn: string * string list -> string list option *)
fun all_except_option(str, []) = NONE
  | all_except_option(str, x::xs) =
    if same_string(str, x) 
    then SOME xs
    else case all_except_option (str, xs) of
             SOME a => SOME (x::a)
           | _ => NONE


(* 1b fn: string list list * string -> string list*)
fun get_substitutions1([], s) = []
  | get_substitutions1 (x::xs, s) =
    case all_except_option(s, x) of
        SOME a => a @ get_substitutions1(xs, s)
      | _ => get_substitutions1(xs, s)

(* 1c fn: string list list * string -> string list*)
fun get_substitutions2(substitutions, s) =
  let fun aux ([], result) = result
        | aux (x::xs, result) =
          case all_except_option(s, x) of
              SOME a => aux(xs, a @ result)
            | _ => aux(xs, result)
  in
      aux(substitutions, [])
  end

(* 1d fn: string list list * {first:string, middle:string, last:string} ->
 {first:string, middle:string, last:string}*)
fun similar_names(substitutions, {first=x, middle=y, last=z}) =
  let
      fun aux([], result) = result
        | aux (l::ls, result) =
          aux(ls, result @ [{first=l,middle=y,last=z}])
  in
      aux (get_substitutions2(substitutions, x), [{first=x,middle=y,last=z}])
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* 2a fn: card -> color*)
fun card_color (Clubs, _) = Black
  | card_color (Spades, _) = Black
  | card_color (Diamonds, _) = Red
  | card_color (Hearts, _) = Red

(* 2b fn: card -> int*)
fun card_value (_, Num a) = a
  | card_value (_, Ace) = 11
  | card_value _ = 10

(* 2c fn: card list * card * exn*)
fun remove_card ([], _, exn) = raise exn
  | remove_card (x::cs, c, exn) =
    if x = c then cs else remove_card(cs, c, exn)

(* 2d fn: card list -> bool*)
fun all_same_color (c1::c2::cs) = card_color(c1) = card_color(c2) andalso all_same_color(c2::cs)
  | all_same_color (c::[]) = true
  | all_same_color [] = true

(* 2e fn: card list -> int*)
fun sum_cards (cs) =
  let
      fun aux ([], sum) = sum
        | aux (x::xs, sum) =
          aux(xs, card_value(x) + sum)
  in
      aux (cs, 0)
  end

(* 2f fn: card list * int -> int*)
fun score (cs, goal) =
  let
      val same_color = all_same_color cs
      val sum = sum_cards cs
      val score = if sum > goal then 3 * (sum - goal) else goal - sum
  in
      if same_color then score div 2 else score
  end

(* 2g fn: card list * move list * int -> int*)
fun officiate (cs, ms, goal) =
  let
      fun aux(moves, hand, deck) =
        let
            val current_score = score(hand, goal)
        in
            case (moves, hand, deck) of
               ([], _, _) => current_score
             | (Draw::_, _, []) => current_score
             | (Draw::moves', _, deck_card::deck) =>
               if current_score > goal
               then current_score
               else aux(moves', deck_card::hand, deck)
             | ((Discard c)::moves', hand, deck) =>
               aux(moves', remove_card(hand, c, IllegalMove), deck)
        end
  in
      aux(ms, [], cs)
  end


(* 3a *)
fun count_aces [] = 0
  | count_aces (x::xs) =
    if card_value x = 11
    then 1 + count_aces(xs)
    else 0 + count_aces(xs)

fun score_challenge (cs, goal) =
  let
      fun aux (count, min) = let
          val current_min = score(cs, goal +  10 * count)
      in
          if count = 0 then min
          else if min < current_min
          then aux (count - 1, min)
          else aux (count - 1, current_min)
      end
  in
      aux (count_aces(cs), score(cs, goal)) 
  end

fun officiate_challenge (cs, move_list, goal) =
  let
      fun aux_count_aces ([], sum, goal) = 0
        | aux_count_aces (l::ls, sum, goal) =
          if sum > goal then 0
          else
              if card_value(l) = 11
              then 1 + aux_count_aces(ls, sum + 1, goal)
              else aux_count_aces(ls, sum + card_value(l), goal)
      fun aux1 (count, min) =
        let
            val new_min = officiate (cs, move_list, goal + 10 * count)
        in
            if count = 0 then min
            else
                if min < new_min
                then aux1(count - 1, min)
                else aux1(count - 1, new_min)
        end
  in
      aux1(aux_count_aces(cs, 0, goal), officiate(cs, [], goal))
  end

(* 3b *)
fun careful_player (card_list, goal) =
  let
      fun get_card_matching_value ([], value) = NONE
        | get_card_matching_value (l::ls, value) =
          if card_value(l) = value
          then SOME l
          else get_card_matching_value(ls, value)

      fun aux (deck, sum, move_list, hand) =
        let
            val cur_score = (hand, goal)
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
      aux(card_list, 0, [], [])
  end
