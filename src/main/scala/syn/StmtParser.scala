package syn

import core.FilePosRange

lazy val parseAssignVarStmt: Parser[Stmt] = for {
  (iden, startRange) <- parseIden
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

def parseDeclareStmt[T, T2 <: AnyVar](c: Char, f: (Pattern[T2], Expr, FilePosRange, FilePosRange) => T): Parser[T] = (for {
  (iden, range) <- parseIden
  _ <- parseSymbol(":" + c)
  expr <- parseExpr
  endRange <- parseSymbol(";")
} yield f(VarPattern(iden, None, range), expr, range, range | endRange)) <|> (for {
  pattern <- parsePattern
  _ <- parseSymbol("" + c)
  expr <- parseExpr
  endRange <- parseSymbol(";")
} yield f(pattern.asInstanceOf[Pattern[T2]], expr, pattern.range, pattern.range | endRange))

lazy val parseTypeStmt: Parser[GlobalStmt] = for {
  startRange <- parseKeyword("type")
  (iden, idenRange) <- parseIden
  _ <- parseSymbol("=")
  typeExpr <- parseType
  endRange <- parseSymbol(";")
} yield TypeGlobalStmt(iden, typeExpr, idenRange, startRange | endRange)

lazy val parseExprStmt: Parser[Stmt] = for {
  expr <- parseExpr
  endRange <- parseSymbol(";")
} yield ExprStmt(expr, expr.range | endRange)

lazy val parseDeclareVar = parseDeclareStmt('=', LocalVarStmt.apply)
lazy val parseDeclareConst = parseDeclareStmt(':', LocalConstStmt.apply)

lazy val parseStmt: Parser[Stmt] = parseAssignVarStmt <|> parseAssignRefStmt <|> parseDeclareVar <|> parseDeclareConst <|> parseExprStmt

lazy val parseDeclareGlobalVar = parseDeclareStmt('=', GlobalVarStmt.apply)
lazy val parseDeclareGlobalConst = parseDeclareStmt(':', GlobalConstStmt.apply)

lazy val parseGlobalStmts: Parser[List[GlobalStmt]] = (parseTypeStmt <|> parseDeclareGlobalVar <|> parseDeclareGlobalConst).many
