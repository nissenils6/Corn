import core.*
import lex.*
import sem.*
import syn.*

import scala.annotation.tailrec
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

val compilerErrorStrings: Array[String] = Array(
  "Another compiler error? I've had enough of these statically typed languages, let's just rewrite my code in python...",
  "Damn it! I just can't seem to figure this one out, perhaps I should get some sleep and try again tomorrow...",
  "Lol! Imagine getting a compiler error, cringe.",
  "More compiler errors? At least I'm free of null pointer exceptions at runtime...",
  "Another day, another compiler error.",
  "Please! No more compiler errors, I'm so tired of strong type systems.",
  "This error can't possibly be my fault, everything in my code is flawless. It must be the compiler that's wrong this time...",
  "Ugh, yet another error from the finicky compiler... the modern languages insert random type conversion whenever needed to make sure the types line up. Why can't this language do that as well?",
  "At this point, I think the compiler is just pranking me. \"Safe language\" my ass.",
  "Phew! This error could've costed me millions if pushed to production. All hours spent trying to satisfy the compiler is really starting to pay off!",
  "Help! My brain is melting. Right now I'm just making random changes until the compiler doesn't complain anymore.",
  "Tired of compiler errors? Fear no more! I've got something for you: Javascript is the next generation programming language. " +
    "Not only is it free of compiler errors, runtime errors are delayed by having erroneous expressions evaluate to 'undefined' instead which propagate through your code. " +
    "Ignore them and call it a day, your co-worker will eventually find out about them, and hopefully fix them, after painfully tracking 'undefined' values throughout the entire codebase.",
  "Who invented the concept of errors? What is the purpose even? Ahh...",
  "Congratulations! You've got yourself yet another compiler error. Here is a random thought: " +
    "If a programming language was designed to have the most information-dense grammar possible, or in other words, compressed to the theoretical maximum, with all encoding space utilized. " +
    "Then it would be impossible for that language to have compiler errors, or even error at all, since each combination of characters as source code input would correspond to exactly one semantically unique and valid program. " +
    "What a wonderful language that must be! (Fizz-buzz in that language would probably look like: \"#h/N]c?'L.%%^!dbP`=\") If you typed something that is, in your opinion, incorrect, then you would still have a valid program, just a different one.",
  "Dude! You're making so many silly errors, even ChatGPT can do better!"
)

def randomCompilerErrorString: String = compilerErrorStrings((Math.random() * compilerErrorStrings.length).toInt)

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

@tailrec
def parseArgs(args: List[(FilePosRange, String)], parsedArgs: ParsedArgs): Either[Error, ParsedArgs] = args match {
  case Nil => Right(parsedArgs)
  case (_, "-file") :: (_, filePath) :: rest => parseArgs(rest, ParsedArgs(Some(filePath), parsedArgs.options))
  case (range, "-file") :: _ => Left(Error.cmdLine(s"Unexpected end of line when parsing input file", range.file.lastRange))
  case (_, option) :: rest if ParsedArgs.OPTIONS.contains(option) => parseArgs(rest, ParsedArgs(parsedArgs.filePath, parsedArgs.options | ParsedArgs.OPTIONS(option)))
  case (range, cmd) :: _ => Left(Error.cmdLine(s"Invalid command '$cmd'", range))
}

def processCmdLine(args: List[String]): Either[Error, (ParsedArgs, File)] = {
  val cmdLine = File("Command Line", args.map {
    case arg if arg.contains(' ') => s"'$arg'"
    case arg => arg
  }.mkString(" "))

  val cmdArgs = args.foldLeft((0, List[(FilePosRange, String)]())) {
    case ((pos, accArgs), arg) if arg.contains(' ') => (pos + arg.length + 3, (FilePosRange(pos + 1, pos + arg.length + 2, cmdLine), arg) :: accArgs)
    case ((pos, accArgs), arg) => (pos + arg.length + 1, (FilePosRange(pos, pos + arg.length, cmdLine), arg) :: accArgs)
  }._2.reverse

  parseArgs(cmdArgs, ParsedArgs()).map((_, cmdLine))
}

def processHelpFlag(parsedArgs: ParsedArgs, cmdLine: File): Either[Error, Unit] = {
  if (parsedArgs.enabled(ParsedArgs.HELP)) {
    println(window("HELP", help))
    Left(Error.exit(cmdLine))
  } else {
    Right(())
  }
}

// if (parsedArgs.enabled(ParsedArgs.HELP)) {
//   println(window("HELP", help))
// }
//
// opt.globalVarInline(optUnit)
// opt.funExprInline(optUnit)
// opt.inlineFunctions(optUnit)
// opt.localVarInline(optUnit)
// opt.deadCodeElimination(optUnit)
// opt.deadBlockElimination(optUnit)
// val asmProgram = optUnit.generateAsm()
// asm.purgeDeadCode(asmProgram)
// val assembly  = asm.assembleX64WindowsWithLinearScan(asmProgram)
//
// if (parsedArgs.enabled(ParsedArgs.COMP_INFO)) println(window("COMPILATION INFO", List(
//   s"Lexical Analysis runtime:                            ${formatTime(0)}",
//   s"Syntax Analysis runtime:                             ${formatTime(0)}",
//   s"Semantic Analysis runtime:                           ${formatTime(0)}",
//   s"Intermediate Representation Generation runtime:      ${formatTime(0)}",
//   s"Code Optimization runtime:                           ${formatTime(0)}",
//   s"Abstract Assembly Generation runtime:                ${formatTime(0)}",
//   s"Register Allocation and Assembly Generation runtime: ${formatTime(0)}"
// ).mkString("\n")))
//
// if (parsedArgs.enabled(ParsedArgs.TOKENS)) {
//   printFile(filePath + ".tokens.txt", tokens.mkString(" "))
// }
//
// if (parsedArgs.enabled(ParsedArgs.AST)) {
//   printFile(filePath + ".ast.txt", parsedFile.mkString("\n\n"))
// }
//
// if (parsedArgs.enabled(ParsedArgs.DEC_AST)) {
//   printFile(filePath + ".dec_ast.txt", module.format(0))
// }
//
// if (parsedArgs.enabled(ParsedArgs.OPT_GRAPH)) {
//   printFile(filePath + ".opt_graph.txt", optUnit.format())
//   Process(s"dot -Tsvg $filePath.opt_graph.txt -o $filePath.opt_graph.svg").!(ProcessLogger(_ => ()))
// }
//
// if (parsedArgs.enabled(ParsedArgs.ABS_ASM)) {
//   printFile(filePath + ".abs_asm.txt", asmProgram.toString)
// }
//
// printFile(filePath + ".asm", assembly)
// Process(s"fasm $filePath.asm $filePath.exe").!(ProcessLogger(_ => ()))
//
// if (parsedArgs.enabled(ParsedArgs.RUN)) {
//   Process(filePath).!
// }

@main def main(args: String*): Unit = (for {
  (parsedArgs, cmdLine) <- processCmdLine(args.toList)
  _ <- processHelpFlag(parsedArgs, cmdLine)
  filePath <- parsedArgs.filePath.toRight(Error.cmdLine(s"No input file provided", cmdFile.lastRange))
  file <- File(filePath).swap.map(t => Error.cmdLine(s"Failed to read input file: ${t.getMessage}", cmdFile.lastRange)).swap
  tokens <- tokenizeFile(file)
  module <- parseFile(tokens, file)
  _ = injectBuiltins(module)
} yield {

}) match {
  case Left(error) if error.errorType != Error.EXIT =>
    println(randomCompilerErrorString)
    println()
    println(error)
  case _ => ()
}

