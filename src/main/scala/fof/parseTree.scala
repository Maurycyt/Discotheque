package fof

import grammar.FOF.{FOFLexer, FOFParser}
import org.antlr.v4.runtime.{CharStream, CommonTokenStream}

def getFOFParser(inputStream: CharStream): FOFParser = {
	val fofLexer = new FOFLexer(inputStream)
	val fofParser = new FOFParser(new CommonTokenStream(fofLexer))

	fofParser.removeErrorListeners()
	fofParser.addErrorListener(new util.ThrowingErrorListener)

	fofParser
}