package matching

import common.*

class ClauseMatcher(
	name: String,
	clause0: Clause[Variable],
	clause1: Clause[Variable],
	cfg: ScoringConfig
) {
	private val variableIDs0 = collectVarNames[Variable](clause0).zipWithIndex.toMap
	private val variableIDs1 = collectVarNames[Variable](clause1).zipWithIndex.toMap
	private val n0 = variableIDs0.size
	private val n1 = variableIDs1.size

	private val quantifiers0 = Array.fill[Quantifier](n0)(Quantifier.Universal)
	variableIDs0.foreach { (name, id) => quantifiers0(id) = clause0.quantifiers(name) }
	private val quantifiers1 = Array.fill[Quantifier](n1)(Quantifier.Universal)
	variableIDs1.foreach { (name, id) => quantifiers1(id) = clause1.quantifiers(name) }

	private val commonSignedPredicates =
		clause0.literals.map(_.signedPredicate) & clause1.literals.map(_.signedPredicate)

	// Takes a set of literals and converts it to a convenient form of a signed predicate
	// and a list of variable IDs; then groups by the signed predicate and keeps only those
	// which are shared between the two clauses.
	private def prepLiteralsForMatching(
		literals: Set[Literal[Variable]], variableIDs: Map[String, Int]
	): Array[((Boolean, String), Array[Vector[Int]])] =
		literals
			.groupMap(_.signedPredicate)(_.relation.args.map(v => variableIDs(v.name)))
			.filter { (k, v) => commonSignedPredicates.contains(k) }
			.map { (k, v) => (k, v.toArray) }
			.toArray
			.sortBy { (k, v) => (v.length, k) }

	private val normalisedRelations0 = prepLiteralsForMatching(clause0.literals, variableIDs0)
	private val normalisedRelations1 = prepLiteralsForMatching(clause1.literals, variableIDs1)
	private val matchedRelations0 = normalisedRelations0
		.map((_, argsList) => Array.fill(argsList.length)(false))

	private val firstMatchingContext = MatchingContext(
		QuotientMatching(n0, n1, quantifiers0, quantifiers1),
		normalisedRelations0,
		normalisedRelations1,
		clause0.literals.size + clause1.literals.size,
	)
	private val firstScore = firstMatchingContext.score

	private var backtrackCounter = 0

	/**
	 * Yields a lazily evaluated iterable of all candidate pairings for this matching context.
	 * Automatically prunes the search space by:
	 * - discarding predicates that would have been considered earlier, based on [[minPairing]],
	 * - only considering pairings if at least one of the argument lists is unsaturated, and
	 * - only considering pairings for those function symbols, which have a matched result.
	 *
	 * @param matchingContext The matching context.
	 * @param minPairing      The minimum pairing to start from, used to prune the search space.
	 * @return An iterable of candidate pairings.
	 */
	def nextPairings(
		matchingContext: MatchingContext,
		minPairing: ((Int, Int), (Int, Int)) = ((-1, -1), (-1, -1))
	): Iterable[((Int, Int), (Int, Int))] = {
		for {
			// For each predicate in the first clause...
			predicateID0 <- matchingContext.normalisedRelations0.indices.view
			if predicateID0 >= minPairing._1._1

			// Find the matching predicate in the second clause
			(negated, predicateName) = matchingContext.normalisedRelations0(predicateID0)._1
			predicateID1 = matchingContext.normalisedRelations1
				.indexWhere(_._1 == (negated, predicateName))
			if predicateID1 >= minPairing._2._1

			// Then, for each argument list of the predicate in the first clause...
			argListID0 <- matchingContext.normalisedRelations0(predicateID0)._2.indices.view
			if argListID0 >= minPairing._1._2

			// And for each argument list of the matching predicate in the second clause...
			argListID1 <- matchingContext.normalisedRelations1(predicateID1)._2.indices.view
			if argListID1 >= minPairing._2._2

			// Predicate is either a relation symbol or (if it's a function symbol) has a matched result
			argList0 = matchingContext.normalisedRelations0(predicateID0)._2(argListID0)
			argList1 = matchingContext.normalisedRelations1(predicateID1)._2(argListID1)
			if predicateName.last != '\'' ||
				matchingContext.quotientMatching.areMatched(argList0.last, argList1.last)

			// At least one of the argument lists is unsaturated
			if !matchingContext.isSaturated(0, (negated, predicateName, argList0)) ||
				!matchingContext.isSaturated(1, (negated, predicateName, argList1))
		} yield {
			((predicateID0, argListID0), (predicateID1, argListID1))
		}
	}

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
		matchingContext: MatchingContext,
		score: Score,
		cfg: ScoringConfig,
		minPairing: ((Int, Int), (Int, Int)) = ((-1, -1), (-1, -1))
	): (MatchingContext, Score) = {
		backtrackCounter += 1
		if backtrackCounter % 100000 == 0 then {
			println(
				s"Backtrack search: ${backtrackCounter / (1000 * 1000)}.${backtrackCounter / (100 * 1000) % 10}M iterations."
			)
		}

		var result = (matchingContext, score)

		//		// For each predicate in the first clause...
		//		val candidatePredicateIDs0 = matchingContext.normalisedRelations0.indices
		//			.filter(_ >= minPairing._1._1)
		//		for predicateID0 <- candidatePredicateIDs0 do {
		//			// Find the matching predicate in the second clause
		//			val (negated, predicateName) = matchingContext.normalisedRelations0(predicateID0)._1
		//			val predicateID1 = matchingContext.normalisedRelations1
		//				.indexWhere(_._1 == (negated, predicateName))
		//
		//			// Then, for each argument list of the predicate in the first clause...
		//			val candidateArgListIDs0 = matchingContext.normalisedRelations0(predicateID0)._2.indices
		//				.filter(_ >= minPairing._1._2 && predicateID1 >= minPairing._2._1)
		//			for argListID0 <- candidateArgListIDs0 do {
		//				val argList0 = matchingContext.normalisedRelations0(predicateID0)._2(argListID0)
		//				// For each argument list of the matching predicate in the second clause...
		//				val candidateArgListIDs1 = matchingContext.normalisedRelations1(predicateID1)._2.indices
		//					.filter(_ >= minPairing._2._2)
		//				for argListID1 <- matchingContext.normalisedRelations1(predicateID1)._2.indices do {
		//					val argList1 = matchingContext.normalisedRelations1(predicateID1)._2(argListID1)
		//					// If at least one of them is unsaturated, try to match them
		//					if !matchingContext.isSaturated(0, (negated, predicateName, argList0))
		//						|| !matchingContext.isSaturated(1, (negated, predicateName, argList1))
		//					then {

		for ((predicateID0, argListID0), (predicateID1, argListID1)) <- nextPairings(
			matchingContext, minPairing
		) do {
			// Try to match them
			val newMatchingContext = matchingContext.withMatch(
				(predicateID0, argListID0), (predicateID1, argListID1)
			)

			// If the new matching context has a better score than the current score... ?
			// Recurse to search further
			val newScore = newMatchingContext.score
			val (newMatching, newResultScore) = backtrackSearch(
				newMatchingContext, newScore, cfg,
				((predicateID0, argListID0), (predicateID1, argListID1))
			)
			if newResultScore.score(cfg) > result._2.score(cfg) then
				result = (newMatching, newResultScore)
		}
		//					}
		//				}
		//			}

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
		val (bestMatchingContext, score) = backtrackSearch(
			firstMatchingContext, Score(
				totalRelations = firstMatchingContext.totalRelations,
				validRelations = 0,
				invalidRelations = 0,
				variableUnions = 0,
				quantifierClashes = 0
			), cfg
		)
		BestMatchingResult(
			name, (clause0, clause1), (variableIDs0, variableIDs1), bestMatchingContext,
			score
		)
	}
}

object ClauseMatcher {
	case class BestMatchingResult(
		name: String,
		clauses: (Clause[Variable], Clause[Variable]),
		variableIDs: (Map[String, Int], Map[String, Int]),
		matchingContext: MatchingContext,
		score: Score
	)
}

def findBestMatching(
	name: String,
	clause0: Clause[Variable],
	clause1: Clause[Variable],
	cfg: ScoringConfig,
): ClauseMatcher.BestMatchingResult = {
	ClauseMatcher(name, clause0, clause1, cfg).findBestMatching
}
