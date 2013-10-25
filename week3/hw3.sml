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

(**** for the challenge problem only ****)

datatype typ = Anything
             | UnitT
             | IntT
             | TupleT of typ list
             | Datatype of string

(**** you can put all your code here ****)
(* 1 *)
val only_capitals = List.filter (fn str => Char.isUpper(String.sub(str, 0)))

(* 2 *)
val longest_string1 =
    List.foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) ""

(* 3 *)
val longest_string2 =
    List.foldl (fn (x, y) => if String.size(x) >= String.size(y) then x else y) ""

(* 4 *)
fun longest_string_helper f =
    List.foldl (fn (x, y) => if f(String.size(x), String.size(y)) then x else y) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(* 5 *)
val longest_capitalized =
    (longest_string_helper (fn (x, y) => x > y)) o
    (List.filter (fn str => Char.isUpper(String.sub(str, 0))))

(* 6 *)
val rev_string = String.implode o rev o String.explode

(* 7 *)
fun first_answer f lst =
    case List.find (fn x => case f x of SOME i => true | NONE => false) lst of
        SOME v => valOf(f v)
      | NONE => raise NoAnswer
(* another version for 7
fun first_answer f lst =
    let fun aux(l, acc) =
            case l of
                [] => acc
              | head::tail => case f head of
                                  NONE => aux(tail, acc)
                                | SOME v => aux([], v::acc)
    in case aux(lst, []) of
           [] => raise NoAnswer
         | head::_ => head
    end
 *)

(* 8 *)
fun all_answers f lst =
    let fun aux (l, acc) =
            case l of
                [] => SOME acc
              | head::tail => case f head of
                                  NONE => raise NoAnswer
                                | SOME v => aux(tail, acc @ v)
    in
        aux(lst, []) handle NoAnswer => NONE
    end

(* 9-a *)
val count_wildcards = g (fn () => 1) (fn str => 0)
(* 9-b *)
val count_wild_and_variable_lengths = g (fn () => 1) (fn str => String.size str)
(* 9-c *)
fun count_some_var (str, ptn) =
    g (fn () => 0) (fn s => if s = str then 1 else 0) ptn

(* 10 *)
fun check_pat pat =
    let fun all_vars ptn =
            case ptn of
                Variable x => [x]
              | TupleP ps => List.foldl (fn (p, i) => i @ (all_vars p)) [] ps
              | ConstructorP(_, p) => all_vars p
              | _ => []
        fun is_uniq lst =
            case lst of
                [] => true
              | head::[] => true
              | head::tail => (List.exists (fn str => str <> head) tail)
                              andalso (is_uniq tail)
    in
        is_uniq(all_vars pat)
    end

(* 11 *)
fun match (va, pat) =
    case (va, pat) of
        (_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const i, ConstP j) => if i = j then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                 then all_answers match (ListPair.zip(vs, ps))
                                 else NONE
      | (Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2
                                                     then match(v, p)
                                                     else NONE
      | (_, _) => NONE

(* 12 *)
fun first_match va plst =
    SOME (first_answer (fn x => match(va, x)) plst) handle NoAnswer => NONE

(* challenge problem *)
(* Given a pattern, find its typ. If pattern is ConstructorP, find the first
typ match from given lst, other matches (if exist) are equivalent, return
named Datatype, if no find raise NoAnswer exception. *)
fun pattern_to_type (lst, pat) =
    case pat of
        UnitP => UnitT
      | ConstP _ => IntT
      | TupleP ps => TupleT (List.map (fn x => pattern_to_type(lst, x)) ps)
      | ConstructorP(str, p) =>
        let fun cons_match x =
                case x of
                    (s, _, pp) => s = str
                                  andalso (pattern_to_type(lst, p) = pp orelse
                                           pattern_to_type(lst, p) = Anything)
        in case List.find cons_match lst of
               SOME (_, a, _) => Datatype a
             | NONE => raise NoAnswer
        end
      | _ => Anything

(* Given two typs, find the more strict typ. "lenient" means the strict typ
that both typs can have. If no such typ, raise NoAnswer exception. *)
fun get_lenient (t1, t2) =
    if t1 = t2
    then t1
    else case (t1, t2) of
             (_, Anything) => t1
           | (Anything, _) => t2
           | (TupleT ps1, TupleT ps2) =>
             if List.length ps1 = List.length ps2
             then
                 case all_answers (fn x => SOME [get_lenient x]
                                           handle NoAnswer => NONE)
                                  (ListPair.zip(ps1, ps2)) of
                     NONE => raise NoAnswer
                   | SOME l => TupleT l
             else raise NoAnswer
           | (_, _) => raise NoAnswer

(* Check the typ of patterns. First find all the typs of given patterns,
if any of them is NONE return NONE, otherwise get the most lenient typ
from all the typs. If no such typ, return NONE. *)
fun typecheck_patterns (lst, ps) =
    let val typs = all_answers (fn x => SOME [pattern_to_type(lst, x)]
                                         handle NoAnswer => NONE) ps
    in
        case typs of
            NONE => NONE
          | SOME tlst => if tlst = []
                         then NONE
                         else SOME (List.foldl get_lenient Anything tlst)
                              handle NoAnswer => NONE
    end
