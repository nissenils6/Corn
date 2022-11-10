import core.{File, FilePos, Error, ErrorGroup}
import gen.AsmGen
import lex.{LexerState, tokenize}
import syn.{GlobalStmt, ParserState, parseGlobalStmts}
import sem.analyzeFile

import java.io.PrintWriter
import scala.languageFeature.implicitConversions

private val ON_DEKSTOP = true

private val PATH = if ON_DEKSTOP then "C:/Users/nisse/OneDrive/Skrivbord/Corn/TestCode" else "C:/Users/nisse/Desktop/Corn/TestCode"
private val SOURCE_PATH = PATH + ".txt"
private val ASSEMBLY_PATH = PATH + ".asm"

def lexFile(filePath: String): ParserState = {
  val fileName = filePath.split('/').last.split('.').head
  val fileContent = io.Source.fromFile(filePath)
  val source = fileContent.getLines().mkString("\n")
  val file = File(fileName, source)
  val (errors, tokens) = tokenize.eval(LexerState(source.toList, FilePos(0, file))).partitionMap(identity)
  if (errors.nonEmpty) throw Error(Error.LEXICAL, errors.head.range.file, errors)
  ParserState(tokens, file)
}

def parseFile(parserState: ParserState): List[GlobalStmt] = parseGlobalStmts(List(), parserState)._1.reverse

def compile(filePath: String): Unit = {
  def printSeparator(): Unit = {
    println()
    println("-" * 128)
    println()
  }

  try {
    val parserState = lexFile(filePath)

    printSeparator()

    println(parserState.tokens.mkString(" "))

    val parsedFile = parseFile(parserState)
    printSeparator()
    println(parsedFile.mkString("\n\n"))

    val module = analyzeFile(parsedFile, parserState.file)
    printSeparator()
    print(module.format(0))

    printSeparator()

    new PrintWriter(ASSEMBLY_PATH) {
      write(AsmGen.toString)
      close()
    }
  } catch {
    case error: (Error | ErrorGroup) =>
      printSeparator()
      print(error)
  }
}

@main def main(): Unit = {
  compile(SOURCE_PATH)
}