package fof

import grammar.FOF.{FOFLexer, FOFParser}
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream}

import java.nio.file.Path

def getCNFParser(inputStream: CharStream): FOFParser = {
	val cnfLexer = new FOFLexer(inputStream)
	val cnfParser = new FOFParser(new CommonTokenStream(cnfLexer))
	cnfParser
}