package matching

import common.Quantifier

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
	val quotientMatching: QuotientMatching[Quantifier],
	val normalisedRelations0: Array[((Boolean, String), Array[Vector[Int]])],
	val normalisedRelations1: Array[((Boolean, String), Array[Vector[Int]])],
	val totalRelations: Int,
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
			normalisedRelations1,
			totalRelations,
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

	private def getRelationContributions = {
		var validCount = 0
		var invalidCount = 0

		for side <- 0 to 1 do {
			val normalisedRelations = if side == 0 then normalisedRelations0 else normalisedRelations1
			for ((sign, name), argsSet) <- normalisedRelations do {
				for args <- argsSet do {
					getContribution(side, (sign, name, args)) match {
						case Contribution.UnSaturated => ()
						case Contribution.Valid => validCount += 1
						case Contribution.InValid => invalidCount += 1
					}
				}
			}
		}

		(validCount, invalidCount)
	}

	private def getQuantifierClashes: Int = {
		// Penalise for each equivalence class with a clashing quantifier.
		val intra0 = (0 until quotientMatching.n0).count { v =>
			v == quotientMatching.find(0)(v) // The vertex is a class representative.
			&& quotientMatching.getQuantifier(0)(v) == Quantifier.Clashing
		}
		val intra1 = (0 until quotientMatching.n1).count { v =>
			v == quotientMatching.find(1)(v) // The vertex is a class representative.
				&& quotientMatching.getQuantifier(1)(v) == Quantifier.Clashing
		}

		// Penalise for matches between classes with clashing quantifiers.
		val inter = (0 until quotientMatching.n0).count { u =>
			// Precompute representative of the match for the vertex.
			val v = quotientMatching.getMatching(0)(u).map(quotientMatching.find(1))
			val uQuant = quotientMatching.getQuantifier(0)(u)
			val vQuant = v.map(quotientMatching.getQuantifier(1))

			// Count the vertex if:
			v.isDefined // The vertex is matched.
				&& u == quotientMatching.find(0)(u) // The vertex is a class representative.
				&& uQuant != Quantifier.Clashing // It's not already clashing.
			  && vQuant.get != Quantifier.Clashing // The match also isn't.
			  && uQuant.clashesWith(vQuant.get) // The quantifiers clash across the match.
		}

		intra0 + intra1 + inter
	}

	def score: Score = {
		val (validCount: Int, invalidCount: Int) = getRelationContributions

		val unionCount = quotientMatching.getSize - quotientMatching.getQuotientsSize

		val quantifierClashes = getQuantifierClashes

		Score(
			totalRelations,
			validCount,
			invalidCount,
			unionCount,
			quantifierClashes
		)
	}
}
