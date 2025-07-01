package matching

import common.{Quantifier, Semigroup}

/**
 * A matching, but between equivalence classes of two sets.
 * Automatically builds equivalence classes, and keeps them minimal.
 *
 * @param n0 The number of elements in the first set.
 * @param n1 The number of elements in the second set.
 * @param sg0 The semigroup for the first set, used to combine elements.
 * @param sg1 The semigroup for the second set, used to combine elements.
 */
class QuotientMatching[T <: Semigroup[T]](
	val n0: Int,
	val n1: Int,
	sg0: Array[T],
	sg1: Array[T]
) {
	private val quotient = Array(FindAndUnion(n0), FindAndUnion(n1))
	private val matched = Array(Array.fill[Option[Int]](n0)(None), Array.fill[Option[Int]](n1)(None))

	// Copies the Quotient Matching and adds matches between x0 from the first set
	// and x1 from the second. If one of the elements is already matched, then the
	// other element is equated with the matched element. The associated semigroup
	// elements are also combined, but the result is only stored in the representative
	// of the equivalence class.
	def withMatches(matches: (Int, Int)*): QuotientMatching[T] = {
		// Prepare the result object.
		val (newSg0, newSg1) = (sg0.clone(), sg1.clone())
		val result = QuotientMatching(n0, n1, newSg0, newSg1)
		matched(0).copyToArray(result.matched(0))
		matched(1).copyToArray(result.matched(1))

		// Handle the matches. Collect unions to be performed in both sets.
		var (unions0, unions1) = (Array.empty[(Int, Int)], Array.empty[(Int, Int)])

		for ((x0, x1) <- matches) do {
			result.matched(0)(x0) match {
				case Some(x0Match) =>
					unions1 +:= (x1, x0Match)
				case None =>
					result.matched(0)(x0) = Some(x1)
			}
			result.matched(1)(x1) match {
				case Some(x1Match) =>
					unions0 +:= (x0, x1Match)
				case None =>
					result.matched(1)(x1) = Some(x0)
			}
		}

		// Perform the unions and combine the semigroup elements.
		result.quotient(0) = quotient(0).withUnions(unions0 *)
		for ((u,v) <- unions0) do {
			val rep = result.quotient(0).find(u)
			newSg0(rep) = newSg0(rep).combine(sg0(u))
			newSg0(rep) = newSg0(rep).combine(sg0(v))
		}
		result.quotient(1) = quotient(1).withUnions(unions1 *)
		for ((u, v) <- unions1) do {
			val rep = result.quotient(1).find(u)
			newSg1(rep) = newSg1(rep).combine(sg1(u))
			newSg1(rep) = newSg1(rep).combine(sg1(v))
		}

		// Return the result.
		result
	}

	def getSize: Int = n0 + n1

	def getQuotientsSize: Int = quotient(0).getQuotientSize + quotient(1).getQuotientSize

	def getQuotient(side: Int): FindAndUnion = quotient(side)

	def find(side: Int)(x: Int): Int = quotient(side).find(x)

	/**
	 * Returns the quantifier for the whole equivalence class.
	 */
	def getQuantifier(side: Int)(x: Int): T = (if side == 0 then sg0 else sg1)(find(side)(x))

	def getMatching(side: Int)(x: Int): Option[Int] = matched(side)(x)
}
