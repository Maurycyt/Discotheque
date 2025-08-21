package matching

import common.*

/**
 * The Controller supports all necessary operations to control the backtracking algorithm
 * in the [[ClauseMatcher]]. Through an iterator, it provides candidates for matches, in
 * the form of a signed predicate, the index of the argument list on side 0, and the index
 * of the argument list on side 1.
 *
 * It supports the connected components optimisation, which means that its first iteration
 * picks a matching from the available ones, but clears the controller upon taking a checkpoint.
 * All matches must thus be unlocked anew.
 */
class ClauseMatcherBacktrackingController(
	private val matchCandidates: Map[SignedPredicate, CheckpointSet[(Int, Int)]],
	private val isFirstIteration: Boolean = true
) extends Iterable[(SignedPredicate, (Int, Int))] {
	private type CMBC = ClauseMatcherBacktrackingController
	private type IT = (SignedPredicate, (Int, Int))

	class CMBCIterator(cmbc: CMBC) extends Iterator[IT] {
		// Auxiliary counter
		private var left = cmbc.matchCandidates.map((sp, cs) => cs.size).sum
		// Extract iterators for each checkpoint set
		private val iterators = cmbc.matchCandidates.map((sp, cs) => (sp, cs.iterator))

		// Get an iterator over signed predicate, and then an iterator over the first checkpoint set
		private val spIterator = iterators.iterator
		private var (sp, csIterator) = spIterator.next

		override def hasNext: Boolean = left > 0

		override def next: IT = {
			left -= 1
			while !csIterator.hasNext do {
				val (sp_, csIterator_) = spIterator.next
				sp = sp_
				csIterator = csIterator_
			}
			(sp, csIterator.next)
		}

		def checkpoint: CMBC = {
			if isFirstIteration then
				// If this is the first iteration, we clear the controller,
				// so that it continues matching only connected components.
				new CMBC(
					iterators.map((sp, csI) => (sp, CheckpointSet[(Int, Int)](Set.empty, csI.getExhausted))),
					isFirstIteration = false
				)
			else
				// Proceed as normal
				new CMBC(iterators.map((sp, csI) => (sp, csI.checkpoint)), isFirstIteration)

		}
	}

	override def iterator: CMBCIterator = CMBCIterator(this)

	def addAll(elements: Map[SignedPredicate, IterableOnce[(Int, Int)]]): CMBC =
		new CMBC(
			matchCandidates.map((sp, cs) => (sp, cs.addAll(elements.getOrElse(sp, Set.empty)))),
			isFirstIteration
		)
}
