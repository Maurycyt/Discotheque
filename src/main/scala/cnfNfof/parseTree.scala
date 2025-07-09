package cnfNfof

import grammar.CNFnFOF.{CNFnFOFLexer, CNFnFOFParser}
import org.antlr.v4.runtime.*

def getCNFnFOFParser(inputStream: CharStream): CNFnFOFParser = {
	val lexer = new CNFnFOFLexer(inputStream)
	val parser = new CNFnFOFParser(new CommonTokenStream(lexer))

	parser.removeErrorListeners()
	parser.addErrorListener(new util.ThrowingErrorListener)

	parser
}