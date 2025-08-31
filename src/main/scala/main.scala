import cnfNfof.getCNFnFOFParser
import fof.{FormulaToClauseVisitor, VariableCollector, getFOFParser}
import matching.{ClauseMatcher, ClausePrinter, findBestMatching}
import org.antlr.v4.runtime.CharStreams

import scala.jdk.CollectionConverters.*
import scala.math.BigDecimal.RoundingMode

@main
def main(args: String*): Unit = {

	// READING ARGUMENTS

	if args.length < 1 then
		throw new IllegalArgumentException(
			"Usage: sbt \"runMain main <path to database> [<valid relation reward>] [<invalid relation penalty>] "
				+ "[<variable union penalty>] [<quantifier clash penalty>]\""
		)

	val filepath: String = args(0)
	val numberOfResults: Int = if args.length >= 2 then args(1).toInt else 3
	val validRelationReward: Double = if args.length >= 3 then args(2).toDouble else 1.0
	val invalidRelationPenalty: Double = if args.length >= 4 then args(3).toDouble else 1.0
	val variableUnionPenalty: Double = if args.length >= 5 then args(4).toDouble else 1.0
	val quantifierClashPenalty: Double = if args.length >= 6 then args(5).toDouble else 1.0

	val cfg = matching.ScoringConfig(
		validRelationReward,
		invalidRelationPenalty,
		variableUnionPenalty,
		quantifierClashPenalty
	)



	// READING FILE

	println(s"\nReading file: $filepath")
	val cnfNfofFormulaListCtx = getCNFnFOFParser(CharStreams.fromFileName(filepath)).formulaList
	println(s"Found ${cnfNfofFormulaListCtx.formulaEntry.size} formulae.")
	val formulaList = cnfNfofFormulaListCtx.formulaEntry.asScala.toSeq
		.map { entry =>
			val name = entry.name.getText
			val clause = (new FormulaToClauseVisitor).visit(entry)._1
			common.Formula(name, common.correctQuantifiers(clause))
		}
		.toSet
	println(s"${formulaList.size} of them are distinct.")



	// TAKING QUERY

	print("\nEnter query clause: ")
	val queryStringBuilder = StringBuilder()
	{
		var continue = true
		while continue do
			val line = scala.io.StdIn.readLine()
			if line == null || line.isEmpty then
				continue = false
			else
				queryStringBuilder.append(line).addOne('\n')
	}
	val queryFormulaCtx = try {
		getCNFnFOFParser(CharStreams.fromString(queryStringBuilder.toString)).formula
	} catch {
		case e: Exception =>
			println(s"Parsing query failed. :(")
			return
	}
	val variables = (new VariableCollector).visit(queryFormulaCtx)
	val queryClause = common.correctQuantifiers(
		FormulaToClauseVisitor(nameMapping = variables.map { n => (n, n) }.toMap)
			.visit(queryFormulaCtx)._1
	)
	println(s"Flattened: $queryClause")
	println



	// RETRIEVAL

	var bestMatchings = List.empty[ClauseMatcher.BestMatchingResult]

	val startTime = System.nanoTime
	for formula <- formulaList do {
		//		println(s"entry clause: $formula")
		val entryClause = formula.clause
		//		println(s"flattened: $entryClause")
		bestMatchings +:= findBestMatching(formula.name, queryClause, entryClause, cfg)
	}
	val endTime = System.nanoTime



	// OUTPUT

	println(s"$numberOfResults best matchings:")
	println

	bestMatchings.sortBy(-_.score.relativeScore(cfg)).take(numberOfResults).filterNot(_.score.score(cfg) <= 0)
		.foreach { result =>
			ClausePrinter.describeBestMatching(result, cfg)
		}

	println(s"Processing took ${
		BigDecimal((endTime - startTime) / 1e6).setScale(2, RoundingMode.HALF_UP)
	} milliseconds."
	)
}
