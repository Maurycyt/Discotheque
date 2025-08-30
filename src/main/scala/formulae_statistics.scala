import cnfNfof.getCNFnFOFParser
import fof.FormulaToClauseVisitor
import org.antlr.v4.runtime.CharStreams
import scala.jdk.CollectionConverters.*
import scala.math.BigDecimal.RoundingMode

@main
def formulae_statistics(args: String*): Unit = {
	if args.length < 1 then
		throw new IllegalArgumentException(
			"Usage: sbt \"runMain formulae_statistics <path to database>\""
		)

	val filepath: String = args(0)

	println(s"Reading file: $filepath")
	val cnfNfofFormulaListCtx = getCNFnFOFParser(CharStreams.fromFileName(filepath)).formulaList
	println(s"Found ${cnfNfofFormulaListCtx.formulaEntry.size} formulae.")
	val formulaList = cnfNfofFormulaListCtx.formulaEntry.asScala.toSeq
		.map { entry =>
			val name = entry.name.getText
			val clause = (new FormulaToClauseVisitor).visit(entry)._1
			common.Formula(name, common.correctQuantifiers(clause))
		}
		.toSet

	println(s"${formulaList.size} distinct formulae loaded.")

	// Compute size of each formula (number of literals)
	val sizes = formulaList.toSeq.map(_.clause.literals.size)

	if sizes.isEmpty then
		println("No formulae found.")
		return

	val min = sizes.min
	val max = sizes.max
	val mean = BigDecimal(sizes.sum.toDouble / sizes.size).setScale(2, RoundingMode.HALF_UP)

	println(s"\nFormula size statistics:")
	println(s"  Min size: $min")
	println(s"  Max size: $max")
	println(s"  Mean size: $mean")

	// Histogram with buckets of 10
	val bucketedHist = sizes.groupBy(size => scala.math.min((size / 10) * 10, 200)).view
		.mapValues(_.size).toMap
	println("\nHistogram of formula sizes (bucketed, size range -> count):")
	bucketedHist.toSeq.sortBy(_._1).foreach { case (bucketStart, count) =>
		val bucketEnd = bucketStart + 9
		println(f"  $bucketStart%2d-$bucketEnd%2d: $count%3d")
	}
}

