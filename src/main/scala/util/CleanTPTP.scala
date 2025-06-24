package util

import java.io.{File, PrintWriter}
import scala.io.Source

object CleanTPTP {
	private def cleanTPTP(text: String, targetPrefix: String): String = {
		var cleaned = text

		val badPrefixes = List(
			"tff", "thf", "fof", "cnf"
		).filterNot(_ == targetPrefix).mkString("|")

		// Step 0: Remove imports
		cleaned = cleaned.replaceAll("""include\(.*\)\.\s*""", "")

		// Step 1: Remove tff and thf formulas
		cleaned = cleaned.replaceAll(s"($badPrefixes)[^.]*\\.\\s*", "")

		// Step 2: Remove comments
		cleaned = cleaned.replaceAll("""%[^\n]*""", "")

		// Step 3: Remove double empty lines
		cleaned = cleaned.replaceAll("""\n\s*\n\s*""", "\n\n")

		// Step 4: Convert cnf or fof formula to custom format
		cleaned = cleaned.replaceAll("""(?s)(cnf|fof)\((\w+),\w*,(.*?)\)\.""", "$1 / $2 : $3")

		cleaned.trim
	}

	def process(text: String, inputPath: String, targetPrefix: String): Unit = {
		val cleaned = cleanTPTP(text, targetPrefix)

		val out = new PrintWriter(new File(inputPath ++ "." ++ targetPrefix))
		try out.println(cleaned) finally out.close()
	}

	def main(args: Array[String]): Unit = {
		if args.length != 1 then {
			println("Usage: scala CleanTPTP.scala <input_file>")
			sys.exit(1)
		}

		val inputPath = args(0)

		val source = Source.fromFile(inputPath)
		val text = try source.mkString finally source.close()

		process(text, inputPath, "cnf")
		process(text, inputPath, "fof")
	}
}
