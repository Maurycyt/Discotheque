package fof

import grammar.FOF.{FOFLexer, FOFParser}
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

def getFOFParser(inputStream: CharStream): FOFParser = {
	val fofLexer = new FOFLexer(inputStream)
	val fofParser = new FOFParser(new CommonTokenStream(fofLexer))

	fofParser.removeErrorListeners()
	fofParser.addErrorListener(new ThrowingErrorListener)

	fofParser
}