
fun is_older ((y1, m1, d1) : int*int*int, (y2, m2, d2) : int*int*int) =
  y1 < y2 orelse (y1 = y2 andalso m1 < m2) orelse (m1 = m2 andalso d1 < d2)

fun number_in_month (l : ((int*int*int)list), month : int) =
  let fun number_in_month_ (xs : ((int*int*int)list)) =
        if null xs
        then 0
        else if (#2 (hd xs)) = month then number_in_month_(tl xs) + 1
        else number_in_month_(tl xs)
  in
      number_in_month_(l)
  end

fun number_in_months (l : ((int*int*int)list), ms : int list) =
  if null ms
  then 0
  else number_in_month(l, hd ms) + number_in_months(l, tl ms)


fun dates_in_month (l : ((int*int*int)list), month : int) =
  let fun dates_in_month_ (xs : ((int*int*int)list)) =
        if null xs
        then []
        else if (#2 (hd xs)) = month then (hd xs) :: dates_in_month_(tl xs)
        else dates_in_month_(tl xs)
  in
      dates_in_month_(l)
  end

fun dates_in_months (l : ((int*int*int)list), ms : int list) =
  if null ms
  then []
  else dates_in_month(l, hd ms) :: dates_in_months(l, tl ms)

fun get_nth (strs : string list, n : int) =
    if n <= 1
    then hd strs
    else get_nth ((tl strs), n-1)

fun date_to_string ((y, m, d) :(int*int*int)) =
  let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, m) ^ " " ^ (Int.toString d) ^ ", " ^ (Int.toString y)
  end

fun number_before_reaching_sum (sum : int, l : int list) =
  let fun reach_sum(acc : int, xs : int list, count : int) =
        if null xs
        then count
        else if ((hd xs) + acc) >= sum then count
        else reach_sum((hd xs) + acc, tl xs, count + 1)
  in
      reach_sum(0, l, 0)
  end

fun what_month (day : int) =
  let val day_of_month = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      1 + number_before_reaching_sum(day, day_of_month)
  end

fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)


fun oldest (dates : (int*int*int)list) =
  if null dates
  then NONE
  else let
    fun oldest_noempty (dates : (int*int*int)list) =
      if null (tl dates)
      then hd dates
      else let val tl_ans = oldest_noempty(tl dates)
        in
          if is_older((hd dates), tl_ans)
          then hd dates
          else tl_ans
        end
  in
    SOME (oldest_noempty dates)
  end

fun unique_month (xs : (int*int*int) list) =
  true
      
fun number_in_months_challenge (dates : ((int*int*int)list), ms : int list) =
  true
