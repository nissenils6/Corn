def lexFile(filePath: String): Option[ParserState] = {
  val fileName = filePath.split('/').last.split('.').head
  val fileContent = io.Source.fromFile(filePath)
  val source = fileContent.getLines().mkString("\n")
  val file = File(fileName, source)
  val filePos = FilePos(0, file)
  val tokens = tokenize.eval(LexerState(source.toList, filePos))
  val errors = tokens.filter(_.isLeft)
  if (errors.nonEmpty) {
    println()
    println(errors.map(_.swap.map(_.toString).getOrElse("")).mkString("\n"))
    None
  } else {
    Some(ParserState(tokens.map(token => token.getOrElse(null)), file))
  }
}

def parseFile(parserState: ParserState): List[GlobalStmt] = parseGlobalStmts(List(), parserState)._1.reverse

def compile(filePath: String): Unit = {
  def printSeparator(): Unit = {
    println()
    println("-" * 128)
    println()
  }

  try for {
    parserState <- lexFile(filePath)
  } {
    printSeparator()

    println(parserState.tokens.mkString(" "))

    printSeparator()

    val parsedFile = parseFile(parserState)
    println(parsedFile.mkString("\n\n"))

    printSeparator()

    val module = analyzeFile(parsedFile, parserState.file)
    print(module.format(0))

    printSeparator()
  } catch {
    case error: Error =>
      print(error.toString)
      printSeparator()
      error.printStackTrace()
  }
}

@main def main(): Unit = {
  compile("C:/Users/nisse/OneDrive/Skrivbord/TestCode.txt")
}