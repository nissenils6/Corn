import core.{Error, ErrorGroup, File, FilePosRange}
import gen.AsmGen
import lex.{LexerState, tokenize}
import sem.analyzeFile
import syn.{GlobalStmt, ParserState, parseFile}

import java.io.PrintWriter
import scala.languageFeature.implicitConversions

private val ON_DEKSTOP = false

private val PATH = if ON_DEKSTOP then "C:/Users/nisse/OneDrive/Skrivbord/Corn/TestCode" else "C:/Users/nisse/Desktop/Corn/TestCode"

private val SOURCE_PATH = PATH + ".txt"
private val ASSEMBLY_PATH = PATH + ".asm"
private val DOT_PATH = PATH + ".dot.txt"

def time[T](name: String, expr: => T): T = {
  val start = System.nanoTime
  val value = expr
  println(f"Time to perform '$name': ${Math.round((System.nanoTime - start) / 1e5) / 1e1} ms")
  value
}

// optimization, while loops, heap allocation, closures, partial application, structs, unions, generics, builtin datatypes, io, for comprehensions, match expressions

@main def main(): Unit = try {
  val file = File(SOURCE_PATH)
  val tokens = time("Lexical Analysis", tokenize(file))
  val parsedFile = time("Syntax Analysis", parseFile(tokens, file))
  val module = time("Semantic Analysis", analyzeFile(parsedFile, file))
  new PrintWriter(ASSEMBLY_PATH) {
    write(AsmGen.toString)
    close()
  }

  val optUnit = module.generateIr()

  optUnit.fns.foreach {
    case fun: opt.CodeFun => println(fun.format())
  }

  println()
  println(List(tokens.mkString(" "), parsedFile.mkString("\n\n"), module.format(0)).mkString("-" * 128 + "\n\n", "\n\n" + "-" * 128 + "\n\n", "\n\n" + "-" * 128))
} catch {
  case error: (Error | ErrorGroup) =>
    error.printStackTrace()
    print("\n" * 8)
    print(error)
}

//@main def main(): Unit = {
//  new PrintWriter(DOT_PATH) {
//    write(opt.fn.format())
//    close()
//  }
//}
