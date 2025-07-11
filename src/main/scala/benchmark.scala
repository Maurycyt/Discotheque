import cnfNfof.getCNFnFOFParser
import common.Quantifier
import fof.FormulaToClauseVisitor
import matching.{ClauseMatcher, ClausePrinter, findBestMatching}
import org.antlr.v4.runtime.CharStreams

import scala.jdk.CollectionConverters.*
import scala.math.BigDecimal.RoundingMode

@main
def benchmark(args: String*): Unit = {

	// READING ARGUMENTS

	if args.length < 1 then
		throw new IllegalArgumentException(
			"Usage: sbt \"runMain benchmark <path to database>\""
		)

	val filepath: String = args(0)



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



	// PREPARING BENCHMARK QUERY

	var varID = 0
	def nextVarID(): String = {
		val id = varID
		varID += 1
		s"X$id"
	}

	val limit = 10
	val topSignedPredicates = util.Statistician
		.topSignedPredicates(formulaList.map(_.clause).toSeq, limit)
	println(s"\nTop $limit signed predicates:${topSignedPredicates.mkString("\n", "\n", "\n")}")
	val benchmarkQuery = common.correctQuantifiers(common.Clause[common.Variable](
		Map.empty,
		topSignedPredicates.flatMap { case (sp, (score, argsNo)) =>
			Seq.fill(2)(
				common.Literal(sp, Vector.fill(argsNo)(common.Variable(nextVarID())))
			)
		}.toSet
	))
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
		val entryClause = formula.clause
		//		println(s"flattened: $entryClause")
		bestMatchings +:= findBestMatching(formula.name, benchmarkQuery, entryClause, cfg)
	}
	val endTime = System.nanoTime



	// OUTPUT

	println(s"Three best matchings:")
	println

	bestMatchings.sortBy(-_.score.relativeScore(cfg)).take(3).filterNot(_.score.score(cfg) <= 0)
		.foreach { result =>
			ClausePrinter.describeBestMatching(result, cfg)
		}

	println(s"Processing took ${
		BigDecimal((endTime - startTime) / 1e6).setScale(2, RoundingMode.HALF_UP)
	} milliseconds."
	)
}
