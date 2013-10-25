use "hw3.sml";

exception TestCaseFailed

(*empty list, NONE or SOME(Anything)? Doesn't matter for autograder*)
val _ = typecheck_patterns([], []) = NONE orelse raise TestCaseFailed

(*examples from doc*)
val _ = typecheck_patterns([], [TupleP [Variable "x", Variable "y"],
                                TupleP [Wildcard, Wildcard]])
  = SOME(TupleT [Anything, Anything]) orelse raise TestCaseFailed

val _ = typecheck_patterns([], [TupleP [Wildcard, TupleP [Wildcard, Wildcard]],
                                TupleP [Wildcard, Wildcard]])
  = SOME(TupleT [Anything, TupleT [Anything, Anything]]) orelse raise TestCaseFailed

(*int test*)
val _ = typecheck_patterns([], [ConstP 5, Wildcard, ConstP 3, Variable "x"])
  = SOME(IntT) orelse raise TestCaseFailed

(*different types*)
val _ = typecheck_patterns([], [ConstP 5, UnitP])
  = NONE orelse raise TestCaseFailed

val _ = typecheck_patterns([("c", "t", IntT)], [ConstructorP("c", ConstP 5), ConstP 5])
  = NONE orelse raise TestCaseFailed

(*tuples of different length*)
val _ = typecheck_patterns([], [TupleP [Wildcard], TupleP [Wildcard, Wildcard]])
  = NONE orelse raise TestCaseFailed

(*tuples with different types*)
val _ = typecheck_patterns([], [TupleP [ConstP 3], TupleP [UnitP]])
  = NONE orelse raise TestCaseFailed

val _ = typecheck_patterns([], [TupleP [Wildcard, ConstP 1], TupleP [Wildcard, TupleP [Wildcard]]])
  = NONE orelse raise TestCaseFailed

(*no constructor*)
val _ = typecheck_patterns([], [ConstructorP("c", Variable "x")])
  = NONE orelse raise TestCaseFailed

(*constructor*)
val _ = typecheck_patterns([("c", "t", TupleT[IntT, Anything])],
                           [ConstructorP("c", TupleP [ConstP 4, Variable "x"])])
  = SOME(Datatype "t") orelse raise TestCaseFailed

val _ = typecheck_patterns([("c1", "t1", UnitT), ("c2", "t2", UnitT)],
                           [ConstructorP("c1", UnitP), ConstructorP("c2", UnitP)])
  = NONE orelse raise TestCaseFailed

val _ = typecheck_patterns([("c1", "t1", UnitT), ("c2", "t2", UnitT)],
                           [ConstructorP("c1", UnitP), ConstructorP("c1", UnitP)])
  = SOME(Datatype "t1") orelse raise TestCaseFailed

(*wrong type of constructor, or should it work?*)
val _ = typecheck_patterns([("c", "t", TupleT[Anything, Anything])],
                           [ConstructorP("c", TupleP [ConstP 4, Variable "x"])])
  = NONE orelse raise TestCaseFailed

(*tuples*)
val _ = typecheck_patterns([("c", "t", IntT)], [
                           TupleP [Wildcard, TupleP [ConstP 3, Wildcard]],
                           TupleP [Wildcard, TupleP [Variable "x", UnitP]],
                           TupleP [ConstructorP("c", ConstP 13), Wildcard]
                           ])
  = SOME(TupleT [Datatype "t", TupleT [IntT, UnitT]]) orelse raise TestCaseFailed

(*two constructors for the same type*)
val _ = typecheck_patterns([("c1", "t", TupleT[IntT, Datatype "t"]), ("c2", "t", UnitT)],
  [ConstructorP("c1", TupleP [ConstP 5, ConstructorP("c2", UnitP)]), ConstructorP("c2", UnitP)])
  = SOME(Datatype "t") orelse raise TestCaseFailed

val _ = typecheck_patterns([("c", "t", TupleT[IntT, Datatype "t"]), ("c", "t", UnitT)],
  [ConstructorP("c", TupleP [ConstP 5, ConstructorP("c", UnitP)]), ConstructorP("c", UnitP)])
  = SOME(Datatype "t") orelse raise TestCaseFailed

val _ = typecheck_patterns([("c1", "t1", TupleT[IntT, Datatype "t2"]), ("c2", "t2", UnitT)],
                           [ConstructorP("c1", TupleP [ConstP 5, ConstructorP("c2", UnitP)]), ConstructorP("c2", UnitP)])
    = NONE orelse raise TestCaseFailed
val _ = typecheck_patterns ([("foo","bar",IntT)], [ConstructorP("foo", Variable "x")])
        = SOME (Datatype "bar") orelse raise TestCaseFailed
val _ = typecheck_patterns ([("foo","bar",Anything)], [ConstructorP("foo", ConstP 3)])
        = NONE orelse raise TestCaseFailed
