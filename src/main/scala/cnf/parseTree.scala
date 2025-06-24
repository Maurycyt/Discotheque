package cnf

import grammar.CNF.{CNFLexer, CNFParser}
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream}

import java.nio.file.Path

def getCNFParser(inputStream: CharStream): CNFParser = {
	val cnfLexer = new CNFLexer(inputStream)
	val cnfParser = new CNFParser(new CommonTokenStream(cnfLexer))
	cnfParser
}