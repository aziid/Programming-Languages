datatype mytype =
         TwoInts of int*int
         | Str of string
         | Pizza

fun f (x: mytype) =
    case x of
        TwoInts(i1, i2) => i1 + i2
      | Str s => String.size s
      | Pizza => 3

datatype exp =
         Constant of int
         | Negate of exp
         | Add of exp * exp
         | Multiply of exp * exp

fun eval (e: exp) =
    case e of
        Constant i => i
      | Negate e0 => ~ (eval e0)
      | Add(e1, e2) => (eval e1) + (eval e2)
      | Multiply(e1, e2) => (eval e1) * (eval e2)

fun max_const (e : exp) =
    let
        fun max_of_two (e1, e2) =
            Int.max(max_const e1, max_const e2)
    in
        case e of
            Constant i => i
          | Negate e0 => max_const e0
          | Add(e1, e2) => max_of_two(e1, e2)
          | Multiply(e1, e2) => max_of_two(e1, e2)
    end

val test_exp = Add(Constant 20, Multiply(Constant 10, Constant 16))
val twenty = max_const test_exp

fun sum_list l =
    case l of
        [] => 0
      | first::rest => first + sum_list rest

fun append (l1, l2) =
    case l1 of
        [] => l2
      | first::rest => first::append(rest, l2)

val a_list = [1,2,3,4]
val ten = sum_list a_list
val new_list = append(a_list, [5,6])

datatype ('a,'b) tree =
         Node of 'a * ('a, 'b) tree * ('a, 'b) tree
       | Leaf of 'b

fun sum_tree t =
    case t of
        Leaf i => i
      | Node(i, left, right) => i + sum_tree left + sum_tree right

val a_tree = Node(1, Node(2, Leaf 3, Leaf 4), Node(5, Leaf 6, Leaf 7))
val twentyeight = sum_tree a_tree

(*Every function takes exactly ONE argument, type checker figures out with
pattern matching!!*)

fun sum_four (x, y, z, w) =
    x + y + z + w

val ten = sum_four(1,2,3,4)

fun rotate_left (x,y,z) =
    (y,z,x)

val two_three_one = rotate_left(1,2,3)

(*Polymorphic equality type*)
fun same_thing (x, y) =
    if x = y then "yes" else "no"

(*pattern matching:  []  -> empty list,
                    x::y -> list with at least one element
                      _  -> everything *)
exception UnmatchedArgumentError
fun zip3 x =
    case x of
        ([],[],[]) => []
      | (hd1::tl1, hd2::tl2, hd3::tl3) => (hd1, hd2, hd3)::zip3(tl1, tl2, tl3)
      | _ => raise UnmatchedArgumentError

val zip3_test = zip3([1,2,3], [4,5,6], [7,8,9])
val zip3_exception = zip3([1,2], [3,4,5], [7])
                     handle UnmatchedArgumentError => [(1,1,1)]

fun unzip3 x =
    case x of
        [] => ([],[],[])
      | (a, b, c)::tl => let val (atl, btl, ctl) = unzip3 tl
                         in
                             (a::atl, b::btl, c::ctl)
                         end

val unzip3_test = unzip3 zip3_test

fun nodecreasing x =
    case x of
        [] => true
      | _::[] => true
      | hd::(neck::tl) => hd <= neck andalso nodecreasing (neck::tl)

val not_true = nodecreasing [1,3,~2]

fun len lst =
    case lst of
        [] => 0
      | _::tl => 1 + len tl

val five = len [1,2,3,4,5]

(*Tail recursion: Recursion optimization with accumulator*)
fun fact x =
    let
        fun aux(n, acc) =
            case n of
                0 => acc
              | num => aux(num - 1, num*acc)
    in
        aux(x, 1)
    end

val twentyfour = fact 4
val onetwenty = fact 5

fun rev lst =
    let
        fun aux (x, acc) =
            case x of
                [] => acc
              | a::b => aux(b, a::acc)
    in
        aux(lst, [])
    end

val rev_test = rev [1,2,3,4]
