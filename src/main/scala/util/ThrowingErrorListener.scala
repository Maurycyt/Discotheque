package util

import org.antlr.v4.runtime.{BaseErrorListener, RecognitionException, Recognizer}

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
