package util

import common.*

object Statistician {
	// Counts the occurrences and argument counts of each signed predicate in a clause.
	private def countSignedPredicates(clause: Clause[Variable]): Map[SignedPredicate, (Int, Int)] = {
		clause.literals
			.map{ l => (l.signedPredicate, l.args.size) }
			.groupBy(_._1)
			.map((key, values) => (key, (values.size, values.head._2)))
	}

	// Heuristically squares the counts to obtain scores.
	private def convertCountsToScores(counts: Map[SignedPredicate, (Int, Int)]): Map[SignedPredicate, (Int, Int)] = {
		counts.map{ case (key, (count, argsNo)) => (key, (count * count, argsNo)) }
	}

	// Combines two score maps by summing the counts for each signed predicate.
	private def combineScores(
		scores1: Map[SignedPredicate, (Int, Int)],
		scores2: Map[SignedPredicate, (Int, Int)]
	): Map[SignedPredicate, (Int, Int)] = {
		scores1 ++ scores2.map { case (k, (v, an)) => k -> (v + scores1.getOrElse(k, (0, 0))._1, an) }
	}

	// Takes a sequence of clauses and returns the top signed predicates with their scores.
	def topSignedPredicates(
		clauses: Seq[Clause[Variable]],
		limit: Int
	): Seq[(SignedPredicate, (Int, Int))] = {
		val scores = clauses
			.map(countSignedPredicates andThen convertCountsToScores)
			.reduce(combineScores)
		scores.toSeq.sortBy(-_._2._1).take(limit)
	}
}
