import common.*
import matching.{ClauseMatcher, ClausePrinter, findBestMatching}

import scala.jdk.CollectionConverters.*
import scala.math.BigDecimal.RoundingMode

@main
def iterationsTest(args: String*): Unit = {

	def getFormula(size: Int): Formula[Variable] = {
		Formula(
			s"f$size",
			correctQuantifiers(Clause[Variable](
				Map.empty,
				(0 until size).map { i =>
					Literal(
						SignedPredicate(false, "s", 2), Vector(Variable(s"X${2 * i}"), Variable(s"X${2 * i + 1}"))
					)
				}.toSet
			)
			)
		)
	}

	for size <- 1 to 10 do {
		val formula1 = getFormula(size)

		for size2 <- 1 to size if size * size2 <= 36 do {
			val formula2 = getFormula(size2)
			val entryClause1 = formula1.clause
			val entryClause2 = formula2.clause
			println(s"Matching ${formula1.name} with ${formula2.name}.")
			println(s"Expecting ${scala.math.pow(2, size * size2)} iterations.")
			val bestMatch = findBestMatching(
				s"${formula1.name} vs ${formula2.name}",
				entryClause1,
				entryClause2,
				matching.ScoringConfig()
			)
			println()
		}
	}
}
