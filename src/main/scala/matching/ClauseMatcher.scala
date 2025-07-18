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
	): Map[SignedPredicate, Array[Vector[Int]]] =
		literals
			.groupMap(_.signedPredicate)(_.args.map(v => variableIDs(v.name)))
			.filter { (k, v) => commonSignedPredicates.contains(k) }
			.map { (k, v) => (k, v.toArray) }

	private val normalisedRelations0 = prepLiteralsForMatching(clause0.literals, variableIDs0)
	private val normalisedRelations1 = prepLiteralsForMatching(clause1.literals, variableIDs1)
	private val matchedRelations0 = normalisedRelations0
		.map((_, argsList) => Array.fill(argsList.length)(false))

	private val startingMatchCandidates = commonSignedPredicates.map { sp =>
		val numOptions0 = normalisedRelations0(sp).length
		val numOptions1 = normalisedRelations1(sp).length
		sp -> CheckpointSet[(Int, Int)](
			(for
				o0 <- 0 until numOptions0
				o1 <- 0 until numOptions1
			yield (o0, o1)).toSet
		)
	}.toMap
	private val startingController =
		if startingMatchCandidates.isEmpty then
			None
		else
			Some(ClauseMatcherBacktrackingController(startingMatchCandidates))

	private val firstMatchingContext = MatchingContext(
		QuotientMatching(n0, n1, quantifiers0, quantifiers1),
		normalisedRelations0,
		normalisedRelations1,
		clause0.literals.size + clause1.literals.size,
	)
	private val firstScore = firstMatchingContext.score

	private var backtrackCounter = 0

	/**
	 * Recursively searches for the best scoring matching between two clauses.
	 *
	 * @param matchingContext The current matching context.
	 * @param score           The score of the current matching. Used to prune the search space by
	 *                        stopping a search branch if it lowers the score.
	 * @param cfg             The score weights' config.
	 * @param controller      The backtracking controller which generates candidate matches.
	 * @return The best found matching and its score.
	 */
	private def backtrackSearch(
		matchingContext: MatchingContext,
		score: Score,
		cfg: ScoringConfig,
		controller: ClauseMatcherBacktrackingController
	): (MatchingContext, Score) = {
		if backtrackCounter % 100000 == 0 then {
			print(
				s"\rBacktrack search: ${backtrackCounter / (1000 * 1000)}.${backtrackCounter / (100 * 1000) % 10}M iterations."
			)
		}
		backtrackCounter += 1

		var result = (matchingContext, score)
		val matchCandidates = controller.iterator

		for {
			// For every matching candidate
			(sp, (argListID0, argListID1)) <- matchCandidates
			// If at least one of the argument lists is unsaturated
			argList0 = normalisedRelations0(sp)(argListID0)
			argList1 = normalisedRelations1(sp)(argListID1)
			if !matchingContext.isSaturated(0, (sp, argList0)) ||
				!matchingContext.isSaturated(1, (sp, argList1))
		} do {
			// Try to match them
			val newMatchingContext = matchingContext.withMatch(sp, argListID0, argListID1)

			// Recurse to search further
			val newScore = newMatchingContext.score
			val (newMatching, newResultScore) = backtrackSearch(
				newMatchingContext, newScore, cfg, matchCandidates.checkpoint
			)
			if newResultScore.score(cfg) > result._2.score(cfg) then
				result = (newMatching, newResultScore)
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
		val (bestMatchingContext, score) = startingController match {
			case Some(sC) =>
				backtrackSearch(
					firstMatchingContext,
					firstScore,
					cfg,
					sC
				)
			case None => (firstMatchingContext, firstScore)
		}

		println(s"\rTook $backtrackCounter iterations.")

		BestMatchingResult(
			name, (clause0, clause1), (variableIDs0, variableIDs1), bestMatchingContext, score
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
