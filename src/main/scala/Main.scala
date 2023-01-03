import core.{CompilerError, Error, ErrorComponent, ErrorGroup, File, FilePosRange}
import lex.{LexerState, tokenize}
import sem.analyzeFile
import syn.{GlobalStmt, ParserState, parseFile}

import core.printFile
import scala.sys.process.{Process, ProcessLogger}

// optimization, while loops, heap allocation, closures, partial application, structs, unions, generics, builtin datatypes, io, for comprehensions, match expressions

val help: String =
  """Usage: corn [options]

Options:
-file <FILE>  Path to the file to be compiled
              A ".asm" file will be generated and then compiled to a ".exe" file
-help         Prints this command usage description to standard output
-comp_info    Prints information about the compilation process to standard output
-run          Runs the program after it has been compiled and assembled
-tokens       Prints all generated tokens from the Lexical Analysis to a ".tokens.txt" file
-ast          Prints the entire generated abstract syntax tree from the Syntax Analysis to a ".ast.txt" file
-dec_ast      Prints the entire generated decorated abstract syntax tree from the Semantic Analysis to a ".dec_ast.txt" file
-opt_graph    Prints the graph of intermediate representation used during optimization to a ".opt_graph.txt" file and compiles it to a ".opt_graph.svg" file
-abs_asm      Prints the abstract assembly generated as an intermediate step of machine code generation to a ".asb_asm.txt" file

All generated files are named after the input file name, but have different suffixes.""".replace("\r\n", "\n")

def time[T](expr: => T): (T, Long) = {
  val start = System.nanoTime
  val value = expr
  (value, System.nanoTime - start)
}

def formatTime(nanos: Long): String = s"${Math.round(nanos / 1e5) / 10} ms"

def window(title: String, string: String): String = {
  val inset = 1

  val lines = string.split('\n')
  val width = lines.map(_.length).max
  val totalWidth = width + (inset * 2)
  val topWidth = totalWidth - title.length

  val topLineLeft = "-" * ((topWidth - (topWidth / 2)) - 1)
  val topLineRight = "-" * ((topWidth / 2) - 1)

  val topLine = s"+$topLineLeft $title $topLineRight+\n"
  val padLine = "|" + " " * totalWidth + "|\n"
  val bottomLine = "+" + "-" * totalWidth + "+\n"
  val contentLines = lines.map(_.padTo(width, ' ')).map("|" + " " * inset + _ + " " * inset + "|\n")
  "\n" + topLine + padLine * inset + contentLines.mkString + padLine * inset + bottomLine
}

case class ParsedArgs(filePath: Option[String] = None, options: Int = 0) {
  def enabled(option: Int): Boolean = (options & option) > 0
}

object ParsedArgs {
  val HELP = 0x01
  val COMP_INFO = 0x02
  val RUN = 0x04
  val TOKENS = 0x08
  val AST = 0x10
  val DEC_AST = 0x20
  val OPT_GRAPH = 0x40
  val ABS_ASM = 0x40

  val OPTIONS: Map[String, Int] = Map("-help" -> HELP, "-comp_info" -> COMP_INFO, "-run" -> RUN, "-tokens" -> TOKENS, "-ast" -> AST, "-dec_ast" -> DEC_AST, "-opt_graph" -> OPT_GRAPH, "-abs_asm" -> ABS_ASM)
}

def parseArgs(args: List[(FilePosRange, String)], parsedArgs: ParsedArgs): ParsedArgs = args match {
  case List() => parsedArgs
  case (_, "-file") :: (_, filePath) :: rest => parseArgs(rest, ParsedArgs(Some(filePath), parsedArgs.options))
  case (range, "-file") :: _ => throw Error.cmdLine(s"Unexpected end of line when parsing input file", range.file.lastRange)
  case (_, option) :: rest if ParsedArgs.OPTIONS.contains(option) => parseArgs(rest, ParsedArgs(parsedArgs.filePath, parsedArgs.options | ParsedArgs.OPTIONS(option)))
  case (range, cmd) :: _ => throw Error.cmdLine(s"Invalid command '$cmd'", range)
}

@main def main(args: String*): Unit = try {
  return syn.testParsingLibrary()

  val cmdFile = File("Command Line", args.map {
    case arg if arg.contains(' ') => s"'$arg'"
    case arg => arg
  }.mkString(" "))

  val cmdArgs = args.foldLeft((0, List[(FilePosRange, String)]())) {
    case ((pos, accArgs), arg) if arg.contains(' ') => (pos + arg.length + 3, (FilePosRange(pos + 1, pos + arg.length + 2, cmdFile), arg) :: accArgs)
    case ((pos, accArgs), arg) => (pos + arg.length + 1, (FilePosRange(pos, pos + arg.length, cmdFile), arg) :: accArgs)
  }._2.reverse

  val parsedArgs = parseArgs(cmdArgs, ParsedArgs())

  if (parsedArgs.enabled(ParsedArgs.HELP)) {
    println(window("HELP", help))
  }

  for {
    filePath <- parsedArgs.filePath
    file = File(filePath)
    (tokens, lexTime) = time(tokenize(file))
    (parsedFile, parseTime) = time(parseFile(tokens, file))
    (module, semTime) = time(analyzeFile(parsedFile, file))
    (optUnit, irTime) = time(module.generateIr())
    (_, optTime) = time({
      opt.globalVarInline(optUnit)
      opt.funExprInline(optUnit)
      opt.inlineFunctions(optUnit)
      opt.localVarInline(optUnit)
      opt.deadCodeElimination(optUnit)
      opt.deadBlockElimination(optUnit)
    })
    (asmProgram, absAsmTime) = time({
      val asmProgram = optUnit.generateAsm()
      asm.purgeDeadCode(asmProgram)
      asmProgram
    })
    (assembly, asmTime) = time(asm.assembleX64WindowsWithLinearScan(asmProgram))
  } {
    if (parsedArgs.enabled(ParsedArgs.COMP_INFO)) println(window("COMPILATION INFO", List(
      s"Lexical Analysis runtime:                            ${formatTime(lexTime)}",
      s"Syntax Analysis runtime:                             ${formatTime(parseTime)}",
      s"Semantic Analysis runtime:                           ${formatTime(semTime)}",
      s"Intermediate Representation Generation runtime:      ${formatTime(irTime)}",
      s"Code Optimization runtime:                           ${formatTime(optTime)}",
      s"Abstract Assembly Generation runtime:                ${formatTime(absAsmTime)}",
      s"Register Allocation and Assembly Generation runtime: ${formatTime(asmTime)}"
    ).mkString("\n")))

    if (parsedArgs.enabled(ParsedArgs.TOKENS)) {
      printFile(filePath + ".tokens.txt", tokens.mkString(" "))
    }

    if (parsedArgs.enabled(ParsedArgs.AST)) {
      printFile(filePath + ".ast.txt", parsedFile.mkString("\n\n"))
    }

    if (parsedArgs.enabled(ParsedArgs.DEC_AST)) {
      printFile(filePath + ".dec_ast.txt", module.format(0))
    }

    if (parsedArgs.enabled(ParsedArgs.OPT_GRAPH)) {
      printFile(filePath + ".opt_graph.txt", optUnit.format())
      Process(s"dot -Tsvg $filePath.opt_graph.txt -o $filePath.opt_graph.svg").!(ProcessLogger(_ => ()))
    }

    if (parsedArgs.enabled(ParsedArgs.ABS_ASM)) {
      printFile(filePath + ".abs_asm.txt", asmProgram.toString)
    }

    printFile(filePath + ".asm", assembly)
    Process(s"fasm $filePath.asm $filePath.exe").!(ProcessLogger(_ => ()))

    if (parsedArgs.enabled(ParsedArgs.RUN)) {
      Process(filePath).!
    }
  }
} catch {
  case error: CompilerError =>
    error.printStackTrace()
    print("\n" * 4)
    print(window("COMPILER ERROR", error.toString))
}
