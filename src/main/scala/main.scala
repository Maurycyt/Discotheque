import cnf.{ClauseMatcher, ConversionVisitor, describeBestMatching, eliminateFunctors, findBestMatching, getCNFParser}
import org.antlr.v4.runtime.CharStreams

import scala.math.BigDecimal.RoundingMode

@main
def main(filepath: String): Unit = {

	// READING FILE

	println(s"\nReading file: $filepath")
	val cnfFormulaListCtx = getCNFParser(CharStreams.fromFileName(filepath)).cnfFormulaList
	println(s"Found ${cnfFormulaListCtx.cnfFormula.size} CNF formulas.")
	val cnfFormulaList = (new cnf.ConversionVisitor).visitCnfFormulaList(cnfFormulaListCtx).toSet
	println(s"${cnfFormulaList.size} of them are distinct.")



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
	val queryClauseCtx = getCNFParser(CharStreams.fromString(queryStringBuilder.toString)).cnfClause
	val queryClauseParsed = (new ConversionVisitor).visitCnfClause(queryClauseCtx)
	val queryClause = eliminateFunctors(queryClauseParsed)
	println(s"Flattened: $queryClause")
	println



	// RETRIEVAL

	var bestMatchings = List.empty[ClauseMatcher.BestMatchingResult]

	val startTime = System.nanoTime
	for cnfFormula <- cnfFormulaList do {
		val entryClause = eliminateFunctors(cnfFormula.clause)
		bestMatchings +:= findBestMatching(cnfFormula.name, queryClause, entryClause)
	}
	val endTime = System.nanoTime



	// OUTPUT

	println(s"Three best matchings:")
	println

	bestMatchings.sortBy(-_.scoreDelta).take(3).filterNot(_.scoreDelta <= 0).foreach { result =>
		describeBestMatching(result)
	}

	println(s"Processing took ${
		BigDecimal((endTime - startTime) / 1e6).setScale(2, RoundingMode.HALF_UP)
	} milliseconds."
	)
}
