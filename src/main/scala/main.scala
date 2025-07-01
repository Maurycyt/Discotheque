import cnf.getCNFParser
import fof.{FormulaToClauseVisitor, VariableCollector, getFOFParser}
import common.eliminateFunctors
import matching.{ClauseMatcher, describeBestMatching, findBestMatching}
import org.antlr.v4.runtime.CharStreams

import scala.math.BigDecimal.RoundingMode

@main
def main(args: String*): Unit = {

	// READING ARGUMENTS

	if args.length < 1 then
		throw new IllegalArgumentException(
			"Usage: sbt \"run <path to database> [<valid relation reward>] [<invalid relation penalty>] "
				+ "[<variable union penalty>] [<quantifier clash penalty>]\""
		)

	val filepath: String = args(0)
	val validRelationReward: Double = if args.length >= 2 then args(1).toDouble else 1.0
	val invalidRelationPenalty: Double = if args.length >= 3 then args(2).toDouble else 1.0
	val variableUnionPenalty: Double = if args.length >= 4 then args(3).toDouble else 1.0
	val quantifierClashPenalty: Double = if args.length >= 5 then args(4).toDouble else 1.0

	val cfg = matching.ScoringConfig(
		validRelationReward,
		invalidRelationPenalty,
		variableUnionPenalty,
		quantifierClashPenalty
	)



	// READING FILE

	println(s"\nReading file: $filepath")
	val formulaList = if filepath.contains(".cnf") then {
		val cnfFormulaListCtx = getCNFParser(CharStreams.fromFileName(filepath)).cnfFormulaList
		println(s"Found ${cnfFormulaListCtx.cnfFormula.size} CNF formulae.")
		(new cnf.ConversionVisitor).visitCnfFormulaList(cnfFormulaListCtx).toSet
	} else {
		val fofFormulaListCtx = getFOFParser(CharStreams.fromFileName(filepath)).fofFormulaList
		println(s"Found ${fofFormulaListCtx.fofFormula.size} FOF formulae.")
		(new fof.ConversionVisitor).visitFofFormulaList(fofFormulaListCtx).toSet
	}
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
		getFOFParser(CharStreams.fromString(queryStringBuilder.toString)).formula
	} catch {
		case e: Exception =>
			println(s"Parsing query failed. :(")
			return
	}
	val variables = (new VariableCollector).visit(queryFormulaCtx)
	val queryClauseParsed =
		FormulaToClauseVisitor(nameMapping = variables.map { n => (n, n) }.toMap)
		.visit(queryFormulaCtx)._1
	val queryClause = common.eliminateFunctors(queryClauseParsed)
	println(s"Flattened: $queryClause")
	println



	// RETRIEVAL

	var bestMatchings = List.empty[ClauseMatcher.BestMatchingResult]

	val startTime = System.nanoTime
	for formula <- formulaList do {
		//		println(s"entry clause: $formula")
		val entryClause = common.eliminateFunctors(formula.clause)
		//		println(s"flattened: $entryClause")
		bestMatchings +:= findBestMatching(formula.name, queryClause, entryClause, cfg)
	}
	val endTime = System.nanoTime



	// OUTPUT

	println(s"Three best matchings:")
	println

	bestMatchings.sortBy(-_.score.relativeScore(cfg)).take(3).filterNot(_.score.score(cfg) <= 0)
		.foreach { result =>
			describeBestMatching(result, cfg)
		}

	println(s"Processing took ${
		BigDecimal((endTime - startTime) / 1e6).setScale(2, RoundingMode.HALF_UP)
	} milliseconds."
	)
}
