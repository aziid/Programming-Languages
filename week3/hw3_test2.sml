use "hw3.sml";

val test1 = only_capitals ["a", "B", "cB", "Dat"] = ["B", "Dat"]

val test2_1 = longest_string1 [] = ""
val test2_2 = longest_string1 ["A", "bc", "xs", "C"] = "bc"

val test3 = longest_string2 ["A", "bc", "xs", "C"] = "xs"

val test4_1 = longest_string3 ["A", "bc", "xs", "C"] = "bc"
val test4_2 = longest_string4 ["A", "bc", "xs", "C"] = "xs"

val test5_1 = longest_capitalized ["a", "bc", "xs", "c"] = ""
val test5_2 = longest_capitalized ["A", "bc", "xs", "C"] = "A"
val test5_3 = longest_capitalized ["a", "bc", "xs", "C"] = "C"

val test6 = rev_string "code" = "edoc"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1, 2, 3, 4, 5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2, 3, 4, 5] = NONE

val test9_a = count_wildcards(TupleP [Wildcard, Variable "s", Wildcard, ConstructorP("x", Wildcard)]) = 3
val test9_b = count_wild_and_variable_lengths(TupleP [Wildcard, Variable "doc"]) = 4
val test9_c = count_some_var("a", TupleP [Variable "a", UnitP, Variable "a"]) = 2

val test10_1 = check_pat(TupleP [Variable "a", UnitP, Variable "a"]) = false
val test10_2 = check_pat(TupleP [Variable "a", UnitP, Variable "b"]) = true

val test11_1 = match(Const 1, TupleP [ConstP 1]) = NONE
val test11_2 = match(Const 1, Variable "cat") = SOME [("cat", Const 1)]

val test12_1 = first_match Unit [UnitP] = SOME []
val test12_2 = first_match Unit [ConstP 6] = NONE

val test13_1 = get_lenient(TupleT[Anything], TupleT[TupleT[Anything, Anything]])
               =TupleT[TupleT[Anything, Anything]]
val test13_2 = pattern_to_type([("c", "t", IntT)], ConstructorP("c", ConstP 5)) = Datatype "t"
val test13_3 = pattern_to_type([("c", "t", IntT)], TupleP [ConstP 5, UnitP]) = TupleT [IntT, UnitT]
val test13_4 = pattern_to_type([], TupleP [ConstP 5, UnitP]) = TupleT [IntT, UnitT]
