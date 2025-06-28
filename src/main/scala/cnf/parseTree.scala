package cnf

import grammar.CNF.{CNFLexer, CNFParser}
import org.antlr.v4.runtime.*

class ThrowingErrorListener extends BaseErrorListener {
	override def syntaxError(
		recognizer: Recognizer[?, ?],
		offendingSymbol: Any,
		line: Int,
		charPositionInLine: Int,
		msg: String,
		e: RecognitionException
	): Unit = {
		throw new RuntimeException(s"Syntax error at $line:$charPositionInLine — $msg")
	}
}

def getCNFParser(inputStream: CharStream): CNFParser = {
	val cnfLexer = new CNFLexer(inputStream)
	val cnfParser = new CNFParser(new CommonTokenStream(cnfLexer))

	cnfParser.removeErrorListeners()
	cnfParser.addErrorListener(new ThrowingErrorListener)

	cnfParser
}