
fun is_older (date1 : int*int*int, date2 : int*int*int) =
  let
    val y1 = #1 date1
    val m1 = #2 date1
    val d1 = #3 date1
    val y2 = #1 date2
    val m2 = #2 date2
    val d2 = #3 date2
  in
    y1 < y2 orelse (y1 = y2 andalso m1 < m2) orelse (m1 = m2 andalso d1 < d2)
  end

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
  else dates_in_month(l, hd ms) @ dates_in_months(l, tl ms)

fun get_nth (strs : string list, n : int) =
    if n <= 1
    then hd strs
    else get_nth ((tl strs), n-1)

fun date_to_string ((y, m, d) :(int*int*int)) =
  let
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
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
  let
    val day_of_month = [31,28,31,30,31,30,31,31,30,31,30,31]
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
  else
    let
      fun oldest_noempty (dates : (int*int*int)list) =
        if null (tl dates)
        then hd dates
        else
          let
            val tl_ans = oldest_noempty(tl dates)
          in
            if is_older((hd dates), tl_ans)
            then hd dates
            else tl_ans
        end
    in
      SOME (oldest_noempty dates)
    end

fun unique_month (olds : int list, news: int list) =
  let
    fun in_list(nums : int list, num : int) =
      if null nums
      then false
      else if num = hd nums
                then true
                else in_list(tl nums, num)
  in
    if null olds
    then news
    else if in_list(news, hd olds)
            then unique_month(tl olds, news)
            else unique_month(tl olds, (hd olds :: news))
  end

fun number_in_months_challenge (dates : ((int*int*int)list), ms : int list) =
  let
    val months = unique_month(ms, [])
  in
    number_in_months(dates, months)
  end

fun dates_in_months_challenge (dates : ((int*int*int)list), ms :int list) =
  let
    val months = unique_month(ms, [])
  in
    dates_in_months(dates, months)
  end

fun reasonable_date (date : (int*int*int)) =
  let
    val y = #1 date
    val m = #2 date
    val d = #3 date
    val normal_year_month = [31,28,31,30,31,30,31,31,30,31,30,31]
    val leap_year_month = [31,29,31,30, 31,30,31,30,31,30,31]
    fun is_leap_year(year : int) =
      year mod 400 = 0 orelse (year mod 4 = 0 andalso (not (year mod 100 = 0)))
    fun day_valid(year : int, month : int, day : int) =
        let
          val day_of_months = if is_leap_year(year) then leap_year_month else normal_year_month
          fun get_nth_(n : int, xs : int list) =
            if n <= 1
            then hd xs
            else get_nth_(n - 1, tl xs)
        in
          day > 0 andalso (day <= get_nth_(month, day_of_months))
        end
  in
    y > 0 andalso m > 0 andalso m <= 12 andalso day_valid(y, m, d)
  end
