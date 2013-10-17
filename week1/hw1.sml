(* int*int*int -> year, month, day *)
fun is_older (pr1 : int*int*int, pr2 : int*int*int) =
    (#1 pr1 < #1 pr2)
    orelse (#1 pr1 = #1 pr2 andalso #2 pr1 < #2 pr2)
    orelse (#1 pr1 = #1 pr2 andalso #2 pr1 = #2 pr2 andalso #3 pr1 < #3 pr2)

fun number_in_month (dl : (int*int*int) list, month : int) =
    if null dl
    then 0
    else
        let
            val rest_dates = number_in_month((tl dl), month)
        in
            if #2 (hd dl) = month
            then 1 + rest_dates
            else rest_dates
        end

fun number_in_months (dl : (int*int*int) list, months : int list) =
    if null months
    then 0
    else
        number_in_month(dl, (hd months)) + number_in_months(dl, (tl months))

fun dates_in_month (dl : (int*int*int) list, month : int) =
    if null dl
    then []
    else
        let
            val rest_dates = dates_in_month((tl dl), month)
        in
            if #2 (hd dl) = month
            then (hd dl)::rest_dates
            else rest_dates
        end

fun dates_in_months (dl : (int*int*int) list, months : int list) =
    if null months
    then []
    else
        dates_in_month(dl, (hd months)) @ dates_in_months(dl, (tl months))

fun get_nth (sl : string list, n : int) =
    if n = 1
    then hd sl
    else get_nth((tl sl), n - 1)

fun date_to_string (dl : int*int*int) =
    let
        val months = ["January", "February", "March", "April", "May", "June",
                      "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 dl) ^ " " ^ Int.toString(#3 dl) ^ ", " ^ Int.toString(#1 dl)
    end

fun number_before_reaching_sum (sum : int, int_list : int list) =
    if hd int_list >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - (hd int_list), (tl int_list))

fun what_month (day : int) =
    let
        val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        1 + number_before_reaching_sum(day, days_in_month)
    end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dl : (int*int*int) list) =
    if null dl
    then NONE
    else
        let
            val rest_oldest = oldest(tl dl)
        in
            if isSome rest_oldest andalso is_older((valOf rest_oldest), (hd dl))
            then rest_oldest
            else SOME (hd dl)
        end


fun sort_list (num_list : int list) =
    let
        fun merge (num : int, num_list : int list) =
            (* num_list is sorted in increasing order *)
            if null num_list
            then num :: []
            else if num < hd num_list  then num :: num_list
            else (hd num_list) :: merge(num, (tl num_list))
    in
        if null num_list
        then []
        else if null (tl num_list) then num_list
        else merge((hd num_list), sort_list(tl num_list))
    end

fun rm_dup (num_list : int list) =
    let
        val sorted_list = sort_list(num_list)
    in
        if null sorted_list
        then []
        else if null (tl sorted_list) then sorted_list
        else
            let
                val rest_no_dup = rm_dup(tl sorted_list)
            in
                if hd sorted_list = hd rest_no_dup
                then rest_no_dup
                else (hd sorted_list) :: rest_no_dup
            end
    end

(*Alternative to remove duplicates in a list without sorting a list*)
(*DOUBLE RECURSION!!!!*)
(* 
fun remove_duplicates (month_value : int, months_list : int list) = 
    if null months_list
    then []
    else if month_value = hd months_list 
    then remove_duplicates (month_value, tl months_list)
    else hd months_list :: remove_duplicates (month_value, tl months_list)

fun remove_duplicates_list (months_list : int list) = 
    if null months_list
    then []
    else hd months_list :: remove_duplicates_list (remove_duplicates (hd months_list, tl months_list))

*)

fun number_in_months_challenge (dl : (int*int*int) list, months : int list) =
    number_in_months(dl, rm_dup(months))

fun dates_in_months_challenge (dl : (int*int*int) list, months : int list) =
    dates_in_months(dl, rm_dup(months))


fun reasonable_date (date : int*int*int) =
    let
        fun is_leap_year (year : int) =
            (year mod 400 = 0) orelse (year mod 100 > 0 andalso year mod 4 = 0)
        val leap_day_limit = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val normal_day_limit = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        fun get_nth (sl : int list, n : int) =
            if n = 1
            then hd sl
            else get_nth((tl sl), n - 1)
    in
        if (#1 date < 1) orelse ((#2 date > 12) orelse (#2 date < 1))
           orelse ((#3 date < 1) orelse (#3 date > 31))
        then false
        else
            if is_leap_year(#1 date)
            then (#3 date) <= get_nth(leap_day_limit, #2 date)
            else (#3 date) <= get_nth(normal_day_limit, #2 date)
    end
