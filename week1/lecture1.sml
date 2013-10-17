(*This is a comment*)
(*
val x = 34;

val y = 17;

val z = (x + y) + (y + 2);

val q = z + 1;

val abs_z = if z < 0 then 0 - z else z;

val simple_abs_z = abs z;
*)
(*
fun pow (x: int, y: int) =
    if y = 0
    then 1
    else x * pow(x, y - 1)

val out = pow(4, 3)


fun cube(x: int) =
    pow(x, 3)

val out = cube(2) *)
fun sum_list (xs : int list) =
    if null xs
    then 0
    else hd xs + sum_list(tl xs)

(* val out = sum_list([1, 2, 3]) *)

fun append (xs : int list, ys : int list) =
    if xs = []
    then ys
    else (hd xs) :: append((tl xs), ys)

fun count_from_1 (x : int) =
    let
        fun count (from : int) =
            if from = x
            then x :: []
            else from :: count(from + 1)
    in
        count(1)
    end

fun max (s : int list) =
    if null s
    then 0
    else if null(tl s)
    then hd s
    else
        let val tl_max = max(tl s)
        in
            if hd(s) > tl_max
            then hd s
            else tl_max
        end
