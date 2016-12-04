fun alternate (xs : int list) =
  let
      fun alternate_ (ns : int list, n : int) =
	if null ns
	then 0
	else if (n mod 2) = 0
	then ~ (hd ns) + alternate_(tl ns, n + 1)
	else hd ns + alternate_(tl ns, n + 1)
  in
      alternate_(xs, 1)
  end

fun min_max (xs : int list) =
  let
      fun min_max_(ns : int list, max : int, min : int) =
	if null ns
	then (max, min)
	else
	let
	    val tmp_max = if hd ns > max then hd ns else max
            val tmp_min = if hd ns < min then hd ns else min
        in
	    min_max_(tl ns, tmp_max, tmp_min)
        end
  in
      min_max_(xs, hd xs, hd xs)
  end

fun cumsum (xs : int list) =
  
