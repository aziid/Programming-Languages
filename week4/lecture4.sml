(* Mutual recursion: state machine *)
fun match lst =
    let fun s_need_one lst =
            case lst of
                [] => true
              | 1::tail => s_need_two tail
              | _ => false
        and  s_need_two lst =
            case lst of
                [] => false
              | 2::tail => s_need_one tail
              | _ => false
    in
        s_need_one lst
    end

(* Mutual recursive datatype *)
datatype t1 = Foo of int | Bar of t2
     and t2 = Baz of string | Quxx of t1

fun no_zeros_or_empty_strings_t1 x =
    case x of
        Foo i => i <> 0
      | Bar y => no_zeros_or_empty_strings_t2 y
and no_zeros_or_empty_strings_t2 x =
    case x of
        Baz str => String.size str <> 0
      | Quxx y => no_zeros_or_empty_strings_t1 y

(* alternative mutual recursion without special struct *)
fun earlier (f, x) =
    case x of
        Foo i => i <> 0
      | Bar y => f y

fun later x =
    case x of
        Baz str => String.size str <> 0
      | Quxx y => earlier(later, y)

(* signature: bindings not in signature can not be used outside of module *)
signature MYLIB =
sig
    val add : int -> int -> int
    val time: int -> int -> int
    (* minus signature hidden, so minus is not available outside of Mylib *)
    (* val minus: int -> int -> int *)
end

(* module *)
structure MyLib :> MYLIB =
struct
fun add x y = x + y
fun minus x y = x - y
fun time x y =
    let fun aux (s, acc) =
            case s of
                0 => acc
              | _ => aux (minus s 1 , add acc y)
    in
        aux(x, 0)
    end
end

signature RATIONAL =
sig
    type rational
    exception BadFrac
    val Whole : int -> rational
    val make_frac : int * int -> rational
    val add : rational * rational -> rational
    val toString : rational -> string
end

structure Rational1 :> RATIONAL =
struct
(* Invariant 1: all denominators > 0
   Invariant 2: rationals kept in reduced form *)
datatype rational = Whole of int | Frac of int * int
exception BadFrac

(* greatest common divider. Assume x and y > 0 *)
fun gcd (x, y) =
    if x = y
    then x
    else if x < y
    then gcd(x, y - x)
    else gcd(y, x)

(* assume y > 0 *)
fun reduce r =
    case r of
        Whole _ => r
      | Frac(x, y) => if x = 0
                      then Whole 0
                      else
                          let val d = gcd(abs x, y) in
                              if d = y
                              then Whole (x div d)
                              else Frac (x div d, y div d)
                          end

(* ban zero denominator and guarantee it > 0 *)
fun make_frac (x, y) =
    if y = 0
    then raise BadFrac
    else if y < 0
    then reduce(Frac(~x, ~y))
    else reduce(Frac(x, y))

(* rational addition, keep invariant *)
fun add (r1, r2) =
    case (r1, r2) of
        (Whole i, Whole j) => Whole (i + j)
      | (Whole i, Frac(x, y)) => Frac(x + i * y, y)
      | (Frac(x, y), Whole j) => Frac(x + j * y, y)
      | (Frac(x, y), Frac(p, q)) => reduce(Frac(x * q + p * y, y * q))

(* string representation of a rational *)
fun toString r =
    case r of
        Whole i => Int.toString i
      | Frac(x, y) => (Int.toString x) ^ "/" ^ (Int.toString y)

end

(* function equivalence *)
fun double x = x + x

fun say_hi_double () =
    (print "hi";
     double)
(* g1 is defined by "fun", so it evaluates say_hi_double when it's called *)
fun g1 y = (say_hi_double ()) y
(* g2 is defined by "val", so it evaluates say_hi_double once when it's
   defined and does not evaluate say_hi_double when it's called *)
val g2 = (say_hi_double ())
