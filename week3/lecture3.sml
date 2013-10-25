(*functional programming*)
(*
1. no mutation
2. functions as values
*)
fun double x = x * 2
fun decr x = x - 1
val a_tuple = (double, decr, decr(double(4)))
(*first-class functions: at same class as variable, hence "first-class"*)

fun n_times (f, n, x) =
    if n = 0
    then x
    else f(n_times(f, n - 1, x))

val x1 = n_times(double, 5, 10)
val x2 = n_times(decr, 20, 78)
val x3 = n_times(tl, 3, [1,2,3,4,5])

(*higher-order-function that is not polymorphic*)
fun times_until_zero (f, x) =
    if x = 0
    then 0
    else 1 + times_until_zero(f, f x)

val x4 = times_until_zero (decr, 10)

(*polymorphic function that is not high-order*)
fun len lst =
    case lst of
        [] => 0
      | _::tail => 1 + len tail

val x5 = len [1,2,3,4,5]

(*anonymous function, as lambda in python? *)
fun triple_n_times (n, x) =
    n_times(fn x => 3*x, n, x)

val x6 = triple_n_times(2, 7)

(*map*)
fun map (f, lst) =
    case lst of
        [] => []
      | head::tail => (f head)::(map(f, tail))

val x7 = map(fn x => x*2, [1,2,3,4])

(*filter*)
fun filter (f, lst) =
    case lst of
        [] => []
      | head::tail => case f head of
                          true => head::filter(f, tail)
                        | false => filter(f, tail)

val x8 = filter(fn x => x mod 2 = 1, [1,2,3,4,5,6])

(* high-order-functions also works for custom data structures *)
datatype exp =
         Constant of int
         | Negate of exp
         | Add of exp * exp
         | Multiply of exp * exp

fun true_for_all_const (f, e) =
    case e of
        Constant x => f x
      | Negate a => true_for_all_const(f, a)
      | Add(a, b) => true_for_all_const(f, a) andalso true_for_all_const(f, b)
      | Multiply(a, b) => true_for_all_const(f, a) andalso true_for_all_const(f, b)

fun all_odd e = true_for_all_const(fn x => x mod 2 = 1, e)
val x9 = Add (Constant 9, Negate(Multiply(Constant 7, Constant 5)))
val x10 = all_odd x9

(* high-order-function returns function *)
fun is_even_odd str =
    case str of
        "even" => (fn x => x mod 2 = 0)
      | "odd" => (fn x => x mod 2 = 1)

val is_even = is_even_odd "even"
val is_odd = is_even_odd "odd"
val x11 = is_odd 9
val x12 = is_even 9

(* lexical scope: function evaluated in the environment where it's defined *)
val a = 1
(* a binds to 1 *)
fun plus_a x = x + a
(* plus_a binds to a function that in the environment it is defined a = 1 *)
val a = 2
(* a now binds to 2, REPL shows <hidden-value> for the older a *)
val b = 3
(* b binds to 3 *)
val c = plus_a(a + b)
(* plus_a evaluated in its definition environment where a = 1 *)

(* higher-order-function with lexical scope *)
fun work_on_two f =
    f(2)

val x13 = 4
fun four_times x = x * x13
val x14 = work_on_two four_times

(* why lexical scope? *)
(* 1. function meaning should be independent on variable name *)
(* 2. functions can be type checked and reasoned where defined *)
(* 3. closures can easily store the data they need *)

(* fold and closures *)
fun fold (f, acc, lst) =
    case lst of
        [] => acc
      | head::tail => fold(f, f(acc, head), tail)

fun sum lst = fold(fn (x, y) => x + y, 0, lst)

fun if_all_positive lst = fold(fn (x, y) => x andalso y > 0, true, lst)

fun num_in_range (lst, lo, hi) =
    fold(fn(x, y) => x + (if y > lo andalso y < hi then 1 else 0), 0, lst)

fun if_all_shorter (lst, str) =
    let val i = String.size str
    in
        fold(fn (x, y) => x andalso String.size y < i, true, lst)
    end

fun if_all_true (lst, f) =
    fold(fn (x, y) => x andalso f(y), true, lst)

fun if_all_shorter_new (lst, str) =
    let val i = String.size str
    in
        if_all_true(lst, fn x => String.size x < i)
    end

(* function composition *)
fun compose (f, g) = fn x => f(g(x))
infix !>
fun x !> f = f(x)
fun sqrt_of_abs x = x !> abs !> Real.fromInt !> Math.sqrt

(* currying *)
(* currying under the hood: closures *)
val is_sorted3 = fn x => fn y => fn z => z >= y andalso y >= x
val x15 = is_sorted3 7 8 9

(* currying with syntactic sugar *)
fun is_sorted3_nicer x y z = z >= y andalso y >= x
val x16 = is_sorted3_nicer 7 8 9

(* partial application *)
fun curry_fold f acc lst =
    case lst of
        [] => acc
      | head::tail => curry_fold f (f(acc, head)) tail

val curry_sum = curry_fold (fn (x, y) => x + y) 0
val x17 = curry_sum [1,2,3]

fun exists predicate lst =
    case lst of
        [] => false
      | head::tail => predicate head orelse exists predicate tail

val exists_zero = exists (fn x => x = 0)
val x18 = exists_zero [1,2,3,4]

val increment_all_by_one = List.map (fn x => x + 1)
val x19 = increment_all_by_one [1,2,3]

val remove_zeros = List.filter (fn x => x <> 0)
val x20 = remove_zeros [1,2,3,0,4]

fun range x y =
    if x > y
    then []
    else x::range (x + 1) y

val countup_from_zero_to = range 0
val x21 = countup_from_zero_to 10

(* curry wrap: switch between curry and tuple *)
fun curry f x y = f(x, y)
fun uncurry f(x,y) = f x y
fun other_curry f x y = f y x

val countdown_to_ten_from = other_curry range 10
val x22 = countdown_to_ten_from 2

(* callbacks *)
val cbs : (int -> unit) list ref = ref []
fun on_key_event f = cbs := f::(!cbs)
fun on_event i =
    let fun loop fs =
            case fs of
                [] => ()
              | head::tail => (head i; loop tail)
    in
        loop (!cbs)
    end

val times_pressed = ref 0
(* push the press times counter into cbs list *)
val _ = on_key_event (fn _ => times_pressed := (!times_pressed) + 1)

(* whenever print_pressed executes, it got pushed into cbs list *)
fun print_pressed i =
    on_key_event (fn j => if i = j
                          then print ("you pressed" ^ Int.toString i ^ "\n")
                          else ())
val _ = print_pressed 4
val _ = print_pressed 7
val _ = print_pressed 11

(* a trivial callback example *)
fun my_callback x =
    print ("function my_callback was called with argument " ^ Int.toString x ^ "\n")

fun caller f x = f x

fun loop lst =
    case lst of
        [] => ()
      | head::tail => (caller my_callback head; loop tail)

val _ = loop [1,2,3,4,5]
