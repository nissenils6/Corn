package syn

import core.FilePosRange

lazy val parseAssignVarStmt: Parser[Stmt] = for {
  (startRange, iden) <- parseIden
  _ <- parseSymbol("=")
  expr <- parseExpr
  endRange <- parseSymbol(";")
} yield AssignVarStmt(iden, expr, startRange | endRange)

lazy val parseAssignRefStmt: Parser[Stmt] = for {
  startRange <- parseSymbol("!")
  refExpr <- parseHighPrec
  _ <- parseSymbol("=")
  expr <- parseExpr
  endRange <- parseSymbol(";")
} yield AssignRefStmt(refExpr, expr, startRange | endRange)

def parseDeclareStmt[T, T2 <: AnyVar](f: (Pattern[T2], Expr, FilePosRange) => T): Parser[T] = (for {
  (range, iden) <- parseIden
  _ <- parseSymbol(":=")
  expr <- parseExpr
  endRange <- parseSymbol(";")
} yield f(VarPattern(iden, None, range), expr, range | endRange)) <|> (for {
  pattern <- parsePattern
  _ <- parseSymbol("=")
  expr <- parseExpr
  endRange <- parseSymbol(";")
} yield f(pattern.asInstanceOf[Pattern[T2]], expr, pattern.range | endRange))

lazy val parseExprStmt: Parser[Stmt] = for {
  expr <- parseExpr
  endRange <- parseSymbol(";")
} yield ExprStmt(expr, expr.range | endRange)

lazy val parseStmt: Parser[Stmt] = parseAssignVarStmt <|> parseAssignRefStmt <|> parseDeclareStmt(VarStmt.apply) <|> parseExprStmt

lazy val parseGlobalStmts: Parser[List[GlobalStmt]] = parseDeclareStmt(VarGlobalStmt.apply).many
