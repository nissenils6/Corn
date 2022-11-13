import core.{Error, ErrorGroup, File, FilePosRange}
import gen.AsmGen
import lex.{LexerState, tokenize, tokenize2}
import sem.analyzeFile
import syn.{GlobalStmt, ParserState, parseGlobalStmts}

import java.io.PrintWriter
import scala.languageFeature.implicitConversions

private val ON_DEKSTOP = false

private val PATH = if ON_DEKSTOP then "C:/Users/nisse/OneDrive/Skrivbord/Corn/TestCode" else "C:/Users/nisse/Desktop/Corn/TestCode"
private val SOURCE_PATH = PATH + ".txt"
private val ASSEMBLY_PATH = PATH + ".asm"

def parseFile(parserState: ParserState): List[GlobalStmt] = parseGlobalStmts(List(), parserState)._1.reverse

def compile(filePath: String): Unit = {
  def printSeparator(): Unit = {
    println()
    println("-" * 128)
    println()
  }

  try {
    val file = File(SOURCE_PATH)
    val tokens = tokenize2(file)

    printSeparator()

    println(tokens.mkString(" "))

//    val parsedFile = parseFile(parserState)
//    printSeparator()
//    println(parsedFile.mkString("\n\n"))
//
//    val module = analyzeFile(parsedFile, parserState.file)
//    printSeparator()
//    print(module.format(0))
//
//    printSeparator()
//
//    new PrintWriter(ASSEMBLY_PATH) {
//      write(AsmGen.toString)
//      close()
//    }
  } catch {
    case error: (Error | ErrorGroup) =>
      printSeparator()
      print(error)
      error.printStackTrace()
  }
}

@main def main(): Unit = {
  compile(SOURCE_PATH)
}
