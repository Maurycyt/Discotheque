package matching

import common.*

class ClauseMatcher(
	name: String,
	clause0: Clause[Variable],
	clause1: Clause[Variable],
	cfg: ScoringConfig
) {

	import ClauseMatcher.*

	private val variableIDs0 = collectVarNames[Variable](clause0).zipWithIndex.toMap
	private val variableIDs1 = collectVarNames[Variable](clause1).zipWithIndex.toMap
	private val n0 = variableIDs0.size
	private val n1 = variableIDs1.size
	private val quantifiers0 = getQuantifiers(clause0, variableIDs0, n0)
	private val quantifiers1 = getQuantifiers(clause1, variableIDs1, n1)

	private val commonSPs = getCommonPredicates(clause0, clause1)
	private val normalisedRelations0 = prepClauseForMatching(clause0, variableIDs0, commonSPs)
	private val normalisedRelations1 = prepClauseForMatching(clause1, variableIDs1, commonSPs)

	private val varToFunction0 = mapVarsToFunctions(normalisedRelations0)
	private val varToFunction1 = mapVarsToFunctions(normalisedRelations1)

	private val startingMatchCandidates =
		getStartingMatchCandidates(commonSPs, normalisedRelations0, normalisedRelations1)
	private val startingController = getStartingController(startingMatchCandidates)

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
			// (Matching already saturated relations yields no points)
			// (although... maybe it could unlock some function symbols...)
			argList0 = normalisedRelations0(sp)(argListID0)
			argList1 = normalisedRelations1(sp)(argListID1)
			if !matchingContext.isSaturated(0, (sp, argList0)) ||
				!matchingContext.isSaturated(1, (sp, argList1))
		} do {
			// Try to match them
			val newMatchingContext = matchingContext.withMatch(sp, argListID0, argListID1)

			// Update the match candidates with unlocked function match candidates
			val unlockedFunctionMatchCandidates = getUnlockedFunctionMatchCandidates(
				newMatchingContext.quotientMatching,
				varToFunction0,
				varToFunction1,
				argList0,
				argList1
			)
			val newController = matchCandidates.checkpoint.addAll(unlockedFunctionMatchCandidates)

			println(s"Matched $sp ${argList0.mkString("(",",",")")} ${argList1.mkString("(",",",")")}")
			println(s"Unlocked: ${unlockedFunctionMatchCandidates.mkString("\n",",\n","\n")}")

			// Recurse to search further
			val newScore = newMatchingContext.score
			val (newMatching, newResultScore) = backtrackSearch(
				newMatchingContext, newScore, cfg, newController /*matchCandidates.checkpoint*/
			)
			if newResultScore.score(cfg) > result._2.score(cfg) then
				result = (newMatching, newResultScore)
		}

		result
	}

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

	// Get the quantifiers of all variables in a clause.
	private def getQuantifiers(
		clause: Clause[Variable],
		variableIDs: Map[String, Int],
		n: Int
	): Array[Quantifier] = {
		val quantifiers = Array.fill[Quantifier](n)(Quantifier.Universal)
		variableIDs.foreach { (name, id) => quantifiers(id) = clause.quantifiers(name) }
		quantifiers
	}

	// Get the common signed predicates in two clauses.
	private def getCommonPredicates(clause0: Clause[Variable], clause1: Clause[Variable]) =
		clause0.literals.map(_.signedPredicate) & clause1.literals.map(_.signedPredicate)

	// Takes a set of literals and converts it to a convenient form of a signed predicate
	// and a list of variable IDs; then groups by the signed predicate and keeps only those
	// which are shared between the two clauses.
	private def prepClauseForMatching(
		clause: Clause[Variable],
		variableIDs: Map[String, Int],
		commonSignedPredicates: Set[SignedPredicate]
	): Map[SignedPredicate, Array[Vector[Int]]] =
		val literals = clause.literals
		literals
			.groupMap(_.signedPredicate)(_.args.map(v => variableIDs(v.name)))
			.filter { (k, v) => commonSignedPredicates.contains(k) }
			.map { (k, v) => (k, v.toArray) }

	// Produces a map of all variables which are the result variables of functions
	// to their sources (signed predicate and argument list ID)
	private def mapVarsToFunctions(
		normalisedRelations: Map[SignedPredicate, Array[Vector[Int]]]
	): Map[Int, (SignedPredicate, Int)] = {
		normalisedRelations
			.filter { (sp, _) => sp.isFunction }
			.map { (sp, argLists) =>
				argLists.zipWithIndex.map { (argList, idx) => (argList.last, (sp, idx)) }
			}
			.flatten
			.toMap
	}

	// Get the match candidates that are available at the beginning of the backtracking process.
	private def getStartingMatchCandidates(
		commonSPs: Set[SignedPredicate],
		normalisedRelations0: Map[SignedPredicate, Array[Vector[Int]]],
		normalisedRelations1: Map[SignedPredicate, Array[Vector[Int]]]
	): Map[SignedPredicate, CheckpointSet[(Int, Int)]] = {
		// Starting candidates do not include function symbols.
		commonSPs
			.filterNot(_.isFunction)
			.map { sp =>
				val numOptions0 = normalisedRelations0(sp).length
				val numOptions1 = normalisedRelations1(sp).length
				sp -> CheckpointSet[(Int, Int)](
					(for
						o0 <- 0 until numOptions0
						o1 <- 0 until numOptions1
					yield (o0, o1)).toSet
				)
			}.toMap
	}

	// Get the starting controller, which may not exist if there are no starting candidates.
	private def getStartingController(
		startingMatchCandidates: Map[SignedPredicate, CheckpointSet[(Int, Int)]]
	): Option[ClauseMatcherBacktrackingController] = {
		if startingMatchCandidates.isEmpty then
			None
		else
			Some(ClauseMatcherBacktrackingController(startingMatchCandidates))
	}

	private def getUnlockedFunctionMatchCandidates(
		qMatching: QuotientMatching[Quantifier],
		varToFunction0: Map[Int, (SignedPredicate, Int)],
		varToFunction1: Map[Int, (SignedPredicate, Int)],
		argList0: Vector[Int],
		argList1: Vector[Int]
	): Map[SignedPredicate, Set[(Int, Int)]] = {
		val classes0 = qMatching.getClasses(0)
		val classes1 = qMatching.getClasses(1)
		val getMatching0 = qMatching.getMatching(0) andThen (_.get) andThen qMatching.find(1)
		val getMatching1 = qMatching.getMatching(1) andThen (_.get) andThen qMatching.find(0)

		val unlocked0 = argList0.toSet
			// For each argument which is a function result
			.filter(varToFunction0.contains)
			// Get the SP, argListID, and matched SPs and argListIDs
			.map { x => (varToFunction0(x), classes1(getMatching0(x)).flatMap(varToFunction1.get)) }
			// Keep only those with the same SP, and convert to the form (SP, id0, id1)
			.flatMap { case ((sp0, alID0), ys) =>
				ys.filter(_._1 == sp0).map { (_, alID1) => (sp0, alID0, alID1) }
			}

		// Same for the other side (except keep in mind orientation of result)
		val unlocked1 = argList1.toSet
			// For each argument which is a function result
			.filter(varToFunction1.contains)
			// Get the SP, argListID, and matched SPs and argListIDs.
			.map { y => (varToFunction1(y), classes0(getMatching1(y)).flatMap(varToFunction0.get)) }
			// Keep only those with the same SP, and convert to the form (SP, id0, id1)
			.flatMap { case ((sp1, alID1), ys) =>
				ys.filter(_._1 == sp1).map { (_, alID0) => (sp1, alID0, alID1) }
			}

		// Join the two results and return
		val result = (unlocked0 ++ unlocked1).groupMap(_._1)((_, id0, id1) => (id0, id1))
		result
	}
}

def findBestMatching(
	name: String,
	clause0: Clause[Variable],
	clause1: Clause[Variable],
	cfg: ScoringConfig,
): ClauseMatcher.BestMatchingResult = {
	ClauseMatcher(name, clause0, clause1, cfg).findBestMatching
}
