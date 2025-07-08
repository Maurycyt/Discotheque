import cnf.getCNFParser
import common.{Quantifier, eliminateFunctors}
import fof.{FormulaToClauseVisitor, VariableCollector, getFOFParser}
import matching.{ClauseMatcher, describeBestMatching, findBestMatching}
import org.antlr.v4.runtime.CharStreams

import scala.math.BigDecimal.RoundingMode

@main
def benchmark(args: String*): Unit = {

	// READING ARGUMENTS

	if args.length < 1 then
		throw new IllegalArgumentException(
			"Usage: sbt \"run <path to database>\""
		)

	val filepath: String = args(0)



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



	// PREPARING BENCHMARK QUERY

	var varID = 0
	def nextVarID(): String = {
		val id = varID
		varID += 1
		s"X$id"
	}

	val limit = 10
	val topSignedPredicates = util.Statistician
		.topSignedPredicates(formulaList.map(_.clause).map(common.eliminateFunctors).toSeq, limit)
	println(s"\nTop $limit signed predicates:${topSignedPredicates.mkString("\n", "\n", "\n")}")
	val benchmarkQuery = common.Clause[common.Variable](
		Map.empty.withDefaultValue(Quantifier.None),
		topSignedPredicates.flatMap { case ((negated, name), (score, argsNo)) =>
			Seq(
				common.Literal(
					negated,
					common.Relation[common.Variable](name, Vector.fill(argsNo)(common.Variable(nextVarID())))
				),
				common.Literal(
					negated,
					common.Relation[common.Variable](name, Vector.fill(argsNo)(common.Variable(nextVarID())))
				)
			)
		}.toSet
	)
	println(s"Benchmark query: $benchmarkQuery")



	// RETRIEVAL

	var bestMatchings = List.empty[ClauseMatcher.BestMatchingResult]
	val cfg = matching.ScoringConfig()

	val startTime = System.nanoTime
	var formulaID = 0
	for formula <- formulaList do {
		formulaID += 1
		println(s"Processing formula $formulaID/${formulaList.size}: ${formula.name} ...")
		//		println(s"entry clause: $formula")
		val entryClause = common.eliminateFunctors(formula.clause)
		//		println(s"flattened: $entryClause")
		bestMatchings +:= findBestMatching(formula.name, benchmarkQuery, entryClause, cfg)
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
