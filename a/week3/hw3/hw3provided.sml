(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	      val r = g f1 f2 
    in
	      case p of
	          Wildcard          => f1 ()
	        | Variable x        => f2 x
	        | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	        | ConstructorP(_,p) => r p
	        | _                 => 0
    end

fun only_capitals xs  =
  List.filter (fn x => Char.isUpper (String.sub(x, 0))) xs

fun longest_string1 xs =
  List.foldl (fn (x, y) => if String.size x > String.size y then x else y) "" xs

fun longest_string2 xs =
  List.foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" xs

fun longest_string_helper f xs =
  List.foldl (fn (x, y) => if f (String.size x, String.size y) then x else y) "" xs

fun longest_string3 xs =
  longest_string_helper op > xs

fun longest_string4 xs =
  longest_string_helper op >= xs

val longest_capitalized =
    longest_string1 o only_capitals

val rev_string =
    String.implode o rev o String.explode

fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x::xs' => case f x of
                    SOME v => v
                  | NONE => first_answer f xs'

fun all_answers f [] = SOME []
  | all_answers f l =
    let
        fun all_answers_aux lst =
          List.foldl
              (fn (x, y) =>
                  case (x, y) of
                      (SOME x, SOME y) => SOME (y@x)
                    | (_, _) => NONE
              ) (SOME []) (List.map f lst)
    in
        all_answers_aux l
    end

val count_wildcards =
    g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths =
    g (fn _ => 1) (fn x => String.size x)

fun count_some_var (s, p) =
  g (fn _ => 0) (fn x => if x = s then 1 else 0) p            

fun check_pat p =
  let fun all_var p =
        case p of Variable x        => [x]
                | TupleP ps         => List.foldl (fn (a,b) => a@b) [] (map all_var ps)
                | ConstructorP(_,p') => all_var p'
                | _ => []
      fun has_repeat lst =
        case lst of (s:: lst') => if (List.exists (fn (x) => x = s) lst') then true
                                  else has_repeat lst'
                  | _ => false
  in
      not ((has_repeat o all_var) p)
  end
      

fun match (v, p) =
  case (p, v) of (Wildcard, _ ) => SOME []
               | (Variable s, v) => SOME [(s, v)]
               | (UnitP, Unit)  => SOME []
               | (ConstP x, Const y) => if x = y then SOME [] else NONE
               | (ConstructorP (s1,p), Constructor (s2,v)) => if s1 = s2 then match(v, p) else NONE
               | (TupleP ps, Tuple vs) => if length(ps) <> length(vs) then NONE 
                                          else all_answers (fn (p,v) => match(v, p)) (ListPair.zip(ps, vs))
               | (_, _) => NONE

fun first_match v pattern_list =
  SOME (first_answer (fn p => match(v, p)) pattern_list)
  handle NoAnswer => NONE

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
