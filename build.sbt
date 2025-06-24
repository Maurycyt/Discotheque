import scala.sys.process.Process

ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "3.7.0"

libraryDependencies += "org.antlr" % "antlr4" % "4.13.2"

val grammars = Seq("CNF", "FOF")
val grammarFileSuffixes = Seq(
	".interp", ".tokens", "BaseVisitor.java", "Lexer.java", "Lexer.interp", "Lexer.tokens",
	"Parser.java", "Visitor.java"
)
def generatedGrammarFiles(baseDirectoryFile: File, grammarName: String): Seq[File] = {
	grammarFileSuffixes.map {
		suffix => baseDirectoryFile / "src" / "main" / "java" / "grammar" / grammarName / (grammarName + suffix)
	}
}

lazy val root = (project in file("."))
	.settings(
		name := "DiscoTheque",
		scalacOptions ++= Seq("-deprecation", "-explain", "-print-lines", "-new-syntax"),

		// Generates the lexers and parsers.
		Compile / sourceGenerators += Def.task {
			val depsClasspath = (Compile / dependencyClasspath).value.map(_.data).mkString(".:", ":", "")
			val inputFiles = grammars.map(grammar => file(s"src/main/resources/$grammar.g4")).toSet

			val cachedFunction = FileFunction.cached(
				streams.value.cacheDirectory / "grammar"
			) { (in: Set[File]) =>
				(for (grammarFile <- in) yield {
					val grammarPath = grammarFile.toPath
					val grammarName = grammarPath.getFileName.toString.stripSuffix(".g4")
					print(s"Generating $grammarName grammar files... ")
					Process(Seq(
						"java", "-cp", depsClasspath, "org.antlr.v4.Tool", grammarPath.toString, "-no-listener",
						"-visitor", "-package", s"grammar.$grammarName", "-o", s"${baseDirectory.value}/src/main/java/grammar/$grammarName"
					)
					).!
					println(s"Done.")

					generatedGrammarFiles(baseDirectory.value, grammarName)
						.filter(_.toString.endsWith(".java"))
				}).flatten
			}

			cachedFunction(inputFiles).toSeq
		},

		Compile / sourceGenerators += Def.task {
			val depsClasspath = (Compile / dependencyClasspath).value.map(_.data).mkString(".:", ":", "")
			IO.write(Path("dependencies.cp").asFile, depsClasspath)
			Seq()
		},

		cleanFiles ++= grammars
			.flatMap(grammarName => generatedGrammarFiles(baseDirectory.value, grammarName))
	)
