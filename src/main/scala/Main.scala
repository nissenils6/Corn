import core.{Error, ErrorGroup, File, FilePosRange}
import gen.AsmGen
import lex.{LexerState, tokenize, tokenize2}
import sem.analyzeFile
import syn.{GlobalStmt, ParserState, parseGlobalStmts}

import java.io.PrintWriter
import scala.languageFeature.implicitConversions

private val ON_DEKSTOP = true

private val PATH = if ON_DEKSTOP then "C:/Users/nisse/OneDrive/Skrivbord/Corn/TestCode" else "C:/Users/nisse/Desktop/Corn/TestCode"

private val SOURCE_PATH = PATH + ".txt"
private val ASSEMBLY_PATH = PATH + ".asm"

@main def main(): Unit = try {
  val file = File(SOURCE_PATH)
  val tokens = tokenize2(file)
  val parsedFile = parseGlobalStmts(List(), ParserState(tokens, file))._1.reverse
  val module = analyzeFile(parsedFile, file)
  new PrintWriter(ASSEMBLY_PATH) {
    write(AsmGen.toString)
    close()
  }

  println(List(tokens.mkString(" "), parsedFile.mkString("\n\n"), module.format(0)).mkString("-" * 128 + "\n\n", "\n" + "-" * 128 + "\n\n", "\n" + "-" * 128))
} catch {
  case error: (Error | ErrorGroup) =>
    error.printStackTrace()
    print("\n" * 8)
    print(error)
}
