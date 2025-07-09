package cnf

import grammar.CNF.{CNFLexer, CNFParser}
import org.antlr.v4.runtime.{CharStream, CommonTokenStream}

def getCNFParser(inputStream: CharStream): CNFParser = {
	val cnfLexer = new CNFLexer(inputStream)
	val cnfParser = new CNFParser(new CommonTokenStream(cnfLexer))

	cnfParser.removeErrorListeners()
	cnfParser.addErrorListener(new util.ThrowingErrorListener)

	cnfParser
}