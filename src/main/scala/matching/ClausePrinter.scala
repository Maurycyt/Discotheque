package matching

import common.{Clause, Variable, Literal}

object ClausePrinter {
	def applyUnification(
		clause: Clause[Variable],
		fnu: FindAndUnion,
		variableIDs: Map[String, Int],
		variablePrefix: String,
	): Clause[Variable] = {
		val variableRenaming = variableIDs.map { (name, id) => (name, variablePrefix + fnu.find(id)) }
		val variableClasses = variableIDs.keySet.groupBy(variableRenaming)
		val combinedQuantifiers = variableClasses.map { (className, names) =>
			(
				className,
				names.map(clause.quantifiers).reduce((q1, q2) => q1.combine(q2))
			)
		}
		Clause(
			combinedQuantifiers,
			clause.literals.map {
				literal => literal.copy(args = literal.args.map(v => Variable(variableRenaming(v.name))))
			}
		)
	}

	def describeBestMatching(
		bestMatching: ClauseMatcher.BestMatchingResult,
		cfg: ScoringConfig,
	): Unit = {
		val ClauseMatcher.BestMatchingResult(
		name, (clause0, clause1), (variableIDs0, variableIDs1), bestMatchingContext, score
		) = bestMatching
		val quotientMatching = bestMatchingContext.quotientMatching
		val numUnions = quotientMatching.getSize - quotientMatching.getQuotientsSize
		println(
			s"Best matching score: $score " +
				s"(weighted total: ${score.score(cfg)}, relative: ${score.relativeScore(cfg)})."
		)
		println(s"\t$name")
		println("First clause after equating:")
		println("\t" + applyUnification(clause0, quotientMatching.getQuotient(0), variableIDs0, "X"))
		println("Second clause after equating:")
		println("\t" + applyUnification(clause1, quotientMatching.getQuotient(1), variableIDs1, "X"))
		println("Variable matching:")
		for xID <- 0 until quotientMatching.n0 do {
			if quotientMatching.find(0)(xID) == xID then {
				val xMatch = quotientMatching.getMatching(0)(xID).map(quotientMatching.find(1))
				val xQuant = quotientMatching.getQuantifier(0)(xID)
				xMatch.foreach { yID =>
					val yQuant = quotientMatching.getQuantifier(1)(yID)
					println(s"\t$xQuant X$xID  <——>  Y$yID $yQuant    (${xQuant combine yQuant})")
				}
			}
		}
		println("Contributions:")
		println((for Literal(sp, args) <- clause0.literals.toVector yield {
			bestMatchingContext.getContribution(0, (sp, args.map(v => variableIDs0(v.name))))
		}).mkString(" | ")
		)
		println((for Literal(sp, args) <- clause1.literals.toVector yield {
			bestMatchingContext.getContribution(1, (sp, args.map(v => variableIDs1(v.name))))
		}).mkString(" | ")
		)
		println
	}
}
