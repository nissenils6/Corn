import java.io.PrintWriter
import scala.languageFeature.implicitConversions

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

    new PrintWriter("C:/Users/nisse/Desktop/Corn/TestCode.asm") {
      write(CodeGen.toString)
      close()
    }
  } catch {
    case error: (Error | ErrorGroup) =>
      printSeparator()
      print(error)
      error.printStackTrace()
  }
}

@main def main(): Unit = {
  compile("C:/Users/nisse/Desktop/Corn/TestCode.txt")
}