package matching

// A matching, but between equivalence classes of two sets.
// Automatically builds equivalence classes, and keeps them minimal.
class QuotientMatching(val n0: Int, val n1: Int) {
	private val quotient = Array(FindAndUnion(n0), FindAndUnion(n1))
	private val matched = Array(Array.fill[Option[Int]](n0)(None), Array.fill[Option[Int]](n1)(None))

	// Copies the Quotient Matching and adds matches between x0 from the first set
	// and x1 from the second. If one of the elements is already matched, then the
	// other element is equated with the matched element.
	def withMatches(matches: (Int, Int)*): QuotientMatching = {
		val result = QuotientMatching(n0, n1)
		matched(0).copyToArray(result.matched(0))
		matched(1).copyToArray(result.matched(1))
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

		result.quotient(0) = quotient(0).withUnions(unions0 *)
		result.quotient(1) = quotient(1).withUnions(unions1 *)

		result
	}

	def getSize: Int = n0 + n1

	def getQuotientsSize: Int = quotient(0).getQuotientSize + quotient(1).getQuotientSize

	def find(side: Int)(x: Int): Int = quotient(side).find(x)

	def getMatching(side: Int)(x: Int): Option[Int] = matched(side)(x)
}
