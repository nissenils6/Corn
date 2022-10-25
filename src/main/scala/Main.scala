def lexFile(filePath: String): Option[ParserState] = {
  val fileName = filePath.split('/').last.split('.').head
  val fileContent = io.Source.fromFile(filePath)
  val source = fileContent.getLines().mkString("\n")
  val file = File(fileName, source)
  val filePos = FilePos(0, file)
  val tokens = tokenize.eval(LexerState(source.toList, filePos))
  val errors = tokens.filter(_.isLeft)
  if (errors.nonEmpty) {
    println(errors.map(_.swap.map(_.toString).getOrElse("")).mkString("\n\n"))
    None
  } else {
    Some(ParserState(tokens.map(token => token.getOrElse(null)), file))
  }
}

def parseFile(parserState: ParserState): List[GlobalStmt] = parseGlobalStmts(List(), parserState)._1.reverse

def compile(filePath: String): Unit = try for {
  parserState <- lexFile(filePath)
} {
  println(parserState.tokens.mkString(" "))
  println()

  println(parseFile(parserState).mkString("\n\n"))
  println()
} catch {
  case error: Error =>
    println(error)
    println()
    error.printStackTrace()
}

val codeGen: CodeGen = CodeGen()

@main def main(): Unit = {
  println()
  compile("C:/Users/nisse/Desktop/TestCode.txt")
}
