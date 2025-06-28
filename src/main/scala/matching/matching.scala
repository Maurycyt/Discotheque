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
		QuotientMatching(n0, n1),
		Array.fill(n0)(Quantifier.Universal),
		Array.fill(n1)(Quantifier.Universal),
		normalisedRelations0,
		normalisedRelations1,
		clause0.literals.size + clause1.literals.size,
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
		matchingContext: MatchingContext,
		score: Score,
		cfg: ScoringConfig,
	): (MatchingContext, Score) = {
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
						val (newMatching, newResultScore) = backtrackSearch(newMatchingContext, newScore, cfg)
						if newResultScore.score(cfg) > result._2.score(cfg) then result = (newMatching, newResultScore)
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
		val (bestMatchingContext, score) = backtrackSearch(firstMatchingContext, Score(
			totalRelations = firstMatchingContext.totalRelations,
			validRelations = 0,
			invalidRelations = 0,
			variableUnions = 0,
			quantifierClashes = 0
		), cfg)
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
	println("\t" + Clause(clause0.literals.map {
		literal =>
			Literal(
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
	println("\t" + Clause(clause1.literals.map {
		literal =>
			Literal(
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
	println((for Literal(negated, Relation(name, args)) <- clause0.literals.toVector yield {
		bestMatchingContext.getContribution(0, (negated, name, args.map(v => variableIDs0(v.name))))
	}).mkString(" | ")
	)
	println((for Literal(negated, Relation(name, args)) <- clause1.literals.toVector yield {
		bestMatchingContext.getContribution(1, (negated, name, args.map(v => variableIDs1(v.name))))
	}).mkString(" | ")
	)
	println
}
