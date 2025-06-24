package cnf

// Find-and-union structure for equivalence classes.
class FindAndUnion(val n: Int) {
	private val parent = Array.tabulate(n)(identity)
	private var quotientSize = n

	// Finds the representative of the equivalence class containing x.
	// Uses path compression.
	def find(x: Int): Int = {
		if parent(x) != x then parent(x) = find(parent(x))
		parent(x)
	}

	// Joins the equivalence classes of x and y.
	private def union(x: Int, y: Int): Unit = {
		val rx = find(x)
		val ry = find(y)
		if rx == ry then return
		parent(rx) = ry
		quotientSize -= 1
	}

	// Copy the find-and-union structure, with additional unions.
	def withUnions(unions: (Int, Int)*): FindAndUnion = {
		val result = FindAndUnion(n)
		parent.copyToArray(result.parent)
		result.quotientSize = quotientSize
		for ((x, y) <- unions) {
			result.union(x, y)
		}
		result
	}

	// Returns the current size of the quotient set.
	def getQuotientSize: Int = quotientSize

	// Copies the parent array to the provided array.
	def copyParentsTo(array: Array[Int]): Unit = {
		parent.copyToArray(array)
	}
}

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

/**
 * Context for matching two clauses to help with the backtracking algorithm.
 *
 * It's slightly asymmetrical, because it only keeps track of which relations of the *first* clause
 * have already been matched (implying that it is undesirable to match them again), while potentially
 * they could be matched with multiple relations in the *second* clause. Perhaps this is desirable,
 * because really how many clauses of the entry should be matched with one clause of the query?
 *
 * @param quotientMatching     The quotient matching structure that keeps track of equivalence classes.
 * @param normalisedRelations0 List of relations of the first clause, grouped by predicate.
 * @param normalisedRelations1 List of relations of the second clause, grouped by predicate.
 */
class MatchingContext(
	val quotientMatching: QuotientMatching,
	val normalisedRelations0: Array[((Boolean, String), Array[Vector[Int]])],
	val normalisedRelations1: Array[((Boolean, String), Array[Vector[Int]])],
) {

	import MatchingContext.*

	/**
	 * Creates a new MatchingContext with an additional match between relations.
	 *
	 * @param relID0 The ID of the relation in the first clause,
	 *               represented by a tuple (predicate index, argument list index).
	 * @param relID1 The ID of the relation in the second clause,
	 *               represented by a tuple (predicate index, argument list index).
	 * @return A new MatchingContext with the extra relation match.
	 */
	def withMatch(relID0: (Int, Int), relID1: (Int, Int)): MatchingContext = {
		assert(
			normalisedRelations0(relID0._1)._1 == normalisedRelations1(relID1._1)._1, "Predicate mismatch"
		)

		val argList0 = normalisedRelations0(relID0._1)._2(relID0._2)
		val argList1 = normalisedRelations1(relID1._1)._2(relID1._2)
		val newQuotientMatching = quotientMatching.withMatches(argList0 zip argList1 *)

		MatchingContext(
			newQuotientMatching,
			normalisedRelations0,
			normalisedRelations1
		)
	}

	// Checks if a relation is saturated, meaning that all its arguments are matched.
	def isSaturated(
		side: Int,
		relation: (Boolean, String, Vector[Int])
	): Boolean = {
		val args = relation._3
		args.forall { arg =>
			quotientMatching.getMatching(side)(arg).isDefined
		}
	}

	// Get the contribution of a single relation from one side of the matching.
	// The side must be either 0 or 1.
	def getContribution(
		side: Int,
		relation: (Boolean, String, Vector[Int])
	): Contribution = {
		val otherRelations = if side == 0 then normalisedRelations1 else normalisedRelations0
		val args = relation._3

		// Check saturation
		val saturated = args.forall { arg =>
			quotientMatching.getMatching(side)(arg).isDefined
		}
		if !saturated then return Contribution.UnSaturated

		// Check validity
		val matchedClasses = args
			.map(
				quotientMatching.getMatching(side) andThen (_.get) andThen quotientMatching.find(1 - side)
			)
		otherRelations.find(_._1 == (relation._1, relation._2)) match {
			case Some((_, otherArgsSet)) =>
				if otherArgsSet.exists { otherArgs =>
					otherArgs.zip(matchedClasses).forall { (otherArg, matchedClass) =>
						quotientMatching.find(1 - side)(otherArg) == matchedClass
					}
				} then Contribution.Valid
				else Contribution.InValid
			case None => Contribution.InValid
		}
	}

	def score: Int = {
		var validityScore = 0

		for side <- 0 to 1 do {
			val normalisedRelations = if side == 0 then normalisedRelations0 else normalisedRelations1
			for ((sign, name), argsSet) <- normalisedRelations do {
				for args <- argsSet do {
					getContribution(side, (sign, name, args)) match {
						case Contribution.UnSaturated => ()
						case Contribution.InValid => validityScore -= 1
						case Contribution.Valid => validityScore += 1
					}
				}
			}
		}

		val quotientScore =
			quotientMatching.getQuotientsSize - (quotientMatching.n0 + quotientMatching.n1)
		validityScore + quotientScore
	}
}

object MatchingContext {
	enum Contribution {
		// Not all arguments in a relation are matched.
		case UnSaturated

		// All the arguments in a relation are matched, but
		// there is no corresponding relation in the other clause.
		case InValid

		// All the arguments in a relation are matched, and
		// there is a corresponding relation in the other clause
		case Valid
	}
}

class ClauseMatcher(name: String, clause0: CNFClause[Variable], clause1: CNFClause[Variable]) {
	private val variableIDs0 = collectVarNames[Variable](clause0).zipWithIndex.toMap
	private val variableIDs1 = collectVarNames[Variable](clause1).zipWithIndex.toMap
	private val n0 = variableIDs0.size
	private val n1 = variableIDs1.size

	private val commonSignedPredicates =
		clause0.literals.map(_.signedPredicate) & clause1.literals.map(_.signedPredicate)

	// Takes a set of literals and converts it to a convenient form of a signed predicate
	// and a list of variable IDs; then groups by the signed predicate and keeps only those
	// which are shared between the two clauses.
	private def prepLiteralsForMatching(
		literals: Set[CNFLiteral[Variable]], variableIDs: Map[String, Int]
	): Array[((Boolean, String), Array[Vector[Int]])] =
		literals
			.groupMap(_.signedPredicate)(_.relation.args.map(v => variableIDs(v.name)))
			.filter { (k, v) => commonSignedPredicates.contains(k) }
			.map { (k, v) => (k, v.toArray) }
			.toArray
			.sortBy { (k, v) => (v.size, k) }

	private val normalisedRelations0 = prepLiteralsForMatching(clause0.literals, variableIDs0)
	private val normalisedRelations1 = prepLiteralsForMatching(clause1.literals, variableIDs1)
	private val matchedRelations0 = normalisedRelations0
		.map((_, argsList) => Array.fill(argsList.size)(false))

	private val firstMatchingContext = MatchingContext(
		QuotientMatching(n0, n1),
		normalisedRelations0,
		normalisedRelations1
	)
	private val firstScore = firstMatchingContext.score

	/**
	 * Recursively searches for the best scoring matching between two clauses.
	 *
	 * Assumes that every match should increase the score. Otherwise, it cuts the backtracking branch.
	 * This is a heuristic, because it could be (?) that a single relation match decreases the score,
	 * but several matches at once would increase it.
	 *
	 * @param matchingContext The current matching context.
	 * @param score           The score of the current matching. Used to prune the search space by
	 *                        stopping a search branch if it lowers the score.
	 * @return The best found matching and its score.
	 */
	private def backtrackSearch(
		matchingContext: MatchingContext, score: Int
	): (MatchingContext, Int) = {
		var result = (matchingContext, score)

		// For each predicate in the first clause...
		for predicateID0 <- matchingContext.normalisedRelations0.indices do {
			// Find the matching predicate in the second clause
			val (negated, predicateName) = matchingContext.normalisedRelations0(predicateID0)._1
			val predicateID1 = matchingContext.normalisedRelations1
				.indexWhere(_._1 == (negated, predicateName))

			// Then, for each argument list of the predicate in the first clause...
			for argListID0 <- matchingContext.normalisedRelations0(predicateID0)._2.indices do {
				val argList0 = matchingContext.normalisedRelations0(predicateID0)._2(argListID0)
				// For each argument list of the matching predicate in the second clause...
				for argListID1 <- matchingContext.normalisedRelations1(predicateID1)._2.indices do {
					val argList1 = matchingContext.normalisedRelations1(predicateID1)._2(argListID1)
					// If at least one of them is unsaturated, try to match them
					if !matchingContext.isSaturated(0, (negated, predicateName, argList0))
						|| !matchingContext.isSaturated(1, (negated, predicateName, argList1))
					then {
						// Try to match them
						val newMatchingContext = matchingContext.withMatch(
							(predicateID0, argListID0), (predicateID1, argListID1)
						)

						// If the new matching context has a better score than the current score... ?
						// Recurse to search further
						val newScore = newMatchingContext.score
						val (newMatching, newResultScore) = backtrackSearch(newMatchingContext, newScore)
						if newResultScore > result._2 then result = (newMatching, newResultScore)
					}
				}
			}
		}

		result
	}

	import ClauseMatcher.BestMatchingResult

	/**
	 * Finds the best matching between the two clauses.
	 *
	 * This is a wrapper around the backtrackSearch method that starts the search
	 * from the initial matching context and score. It corrects the final score to account
	 * for the trivially obtainable score from no matches.
	 *
	 * @return The variable translations, the best matching context and its score delta.
	 */
	def findBestMatching: BestMatchingResult = {
		val (bestMatchingContext, score) = backtrackSearch(firstMatchingContext, firstScore)
		BestMatchingResult(
			name, (clause0, clause1), (variableIDs0, variableIDs1), bestMatchingContext,
			score - firstScore
		)
	}
}

object ClauseMatcher {
	case class BestMatchingResult(
		name: String,
		clauses: (CNFClause[Variable], CNFClause[Variable]),
		variableIDs: (Map[String, Int], Map[String, Int]),
		matchingContext: MatchingContext,
		scoreDelta: Int
	)
}

def findBestMatching(
	name: String,
	clause0: CNFClause[Variable],
	clause1: CNFClause[Variable]
): ClauseMatcher.BestMatchingResult = {
	ClauseMatcher(name, clause0, clause1).findBestMatching
}

def describeBestMatching(
	bestMatching: ClauseMatcher.BestMatchingResult,
): Unit = {
	val ClauseMatcher.BestMatchingResult(
	name, (clause0, clause1), (variableIDs0, variableIDs1), bestMatchingContext, scoreDelta
	) = bestMatching
	val quotientMatching = bestMatchingContext.quotientMatching
	val numUnions = quotientMatching.getSize - quotientMatching.getQuotientsSize
	println(
		s"Best matching score delta: $scoreDelta ($numUnions union${if numUnions == 1 then "" else "s"})."
	)
	println(s"\t$name")
	println("First clause after equating:")
	println("\t" + CNFClause(clause0.literals.map {
		literal =>
			CNFLiteral(
				negated = literal.negated,
				relation = Relation(
					name = literal.relation.name,
					args = literal.relation.args
						.map(v => Variable("X" + quotientMatching.find(0)(variableIDs0(v.name))))
				)
			)
	}
	)
	)
	println("Second clause after equating:")
	println("\t" + CNFClause(clause1.literals.map {
		literal =>
			CNFLiteral(
				negated = literal.negated,
				relation = Relation(
					name = literal.relation.name,
					args = literal.relation.args
						.map(v => Variable("Y" + quotientMatching.find(1)(variableIDs1(v.name))))
				)
			)
	}
	)
	)
	println("Variable matching:")
	for xID <- 0 until quotientMatching.n0 do {
		if quotientMatching.find(0)(xID) == xID then {
			val xMatch = quotientMatching.getMatching(0)(xID)
			xMatch.foreach { yID =>
				println(s"\tX$xID  <——>  Y${quotientMatching.find(1)(yID)}")
			}
		}
	}
	println("Contributions:")
	println((for CNFLiteral(negated, Relation(name, args)) <- clause0.literals.toVector yield {
		bestMatchingContext.getContribution(0, (negated, name, args.map(v => variableIDs0(v.name))))
	}).mkString(" | ")
	)
	println((for CNFLiteral(negated, Relation(name, args)) <- clause1.literals.toVector yield {
		bestMatchingContext.getContribution(1, (negated, name, args.map(v => variableIDs1(v.name))))
	}).mkString(" | ")
	)
	println
}
