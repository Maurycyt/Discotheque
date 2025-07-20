import matching.CheckpointSet

val startingSet = Set(1,2)
val cs0 = CheckpointSet(startingSet)

// Check iteration and multiple iteration.
val set0 = (for x <- cs0 yield x).toSet
assert(set0 == startingSet)
val set0a = (for x <- cs0 yield x).toSet
assert(set0a == startingSet)

// Check checkpointing.
val iter0 = cs0.iterator
val x1 = iter0.next
val cs1 = iter0.checkpoint
val set1 = (for x <- cs1 yield x).toSet
assert(set1 == startingSet - x1)

// Recheck iteration after checkpointing.
val set0b = (for x <- cs0 yield x).toSet
assert(set0b == startingSet)

// Check adding elements, including exhausted elements.
val newSet = Set(x1, 3)
val totalSet = startingSet ++ newSet
val cs2 = cs1.addAll(newSet)
val set2 = (for x <- cs2 yield x).toSet
assert(set2 == totalSet - x1)
