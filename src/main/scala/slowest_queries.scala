import cnfNfof.getCNFnFOFParser
import fof.FormulaToClauseVisitor
import matching.ScoringConfig
import org.antlr.v4.runtime.CharStreams

import scala.jdk.CollectionConverters.*
import scala.util.Using
import java.time.Duration
import java.time.Instant

@main
def slowest_queries(args: String*): Unit = {
	if args.length < 1 then
		throw new IllegalArgumentException(
			"Usage: sbt \"runMain slowest_queries <path to database>\""
		)

	val filepath: String = args(0)
	println(s"Reading file: $filepath")
	val cnfNfofFormulaListCtx = getCNFnFOFParser(CharStreams.fromFileName(filepath)).formulaList
	val formulaList = cnfNfofFormulaListCtx.formulaEntry.asScala.toSeq
		.map { entry =>
			val name = entry.name.getText
			val clause = (new FormulaToClauseVisitor).visit(entry)._1
			common.Formula(name, common.correctQuantifiers(clause))
		}
		.distinct

	val formulaListSize = formulaList.size
	println(s"Loaded $formulaListSize formulae.")

	// For each entry, use it as a query and match against all others, timing the search
	val timings = formulaList.zipWithIndex.map { case (query, idx) =>
		print(s"\r$idx/$formulaListSize: ${query.name}...")
		val start = Instant.now()
		// Simulate search: match query against all entries (including itself)
		formulaList.foreach { candidate =>
			matching.ClauseMatcher(candidate.name, query.clause, candidate.clause, ScoringConfig())
				.findBestMatching
		}
		val end = Instant.now()
		val duration = Duration.between(start, end).toMillis
		(query.name, duration, idx)
	}

	// Sort by duration descending
	val slowest = timings.sortBy(-_._2).take(100)

	println("\rTop 100 slowest queries:")
	slowest.foreach { case (name, duration, idx) =>
		println(f"#${idx + 1}%4d: $name%-30s took $duration ms")
	}
}

