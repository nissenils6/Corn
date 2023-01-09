package sem

//private def trivialTypeOf[T <: AnyVar](container: ConstAndTypeContainer, pattern: Pattern[T]): Either[Option[CompilerError], Datatype] = pattern match {
//  case VarPattern(_, typeExprOpt, _) => for {
//    typeExpr <- typeExprOpt.toRight(None)
//    datatype <- resolveTypeExpr(container, typeExpr).mapLeft(Some.apply)
//  } yield datatype
//  case TuplePattern(elements, _) => elements.map(p => trivialTypeOf(container, p)).extract.mapBoth(reduceOptionalErrors, datatypes => TupleDatatype(datatypes, false))
//}
//
//private def trivialTypeof(container: ConstAndTypeContainer, expr: Expr): Either[Option[CompilerError], Datatype] = expr match {
//  case CallExpr(function, _, _) => for {
//    funType <- trivialTypeof(container, function)
//    newType <- funType match {
//      case FunDatatype(_, returnType, _) => Right(returnType)
//      case datatype => Left(Some(Error.semantic(s"Expected the expression to be invoked to be of a function type, found expression of type '$datatype'", function.range)))
//    }
//  } yield newType
//  case IdenExpr(_, _) => Left(None)
//  case RefExpr(expr, _) => for {
//    exprType <- trivialTypeof(container, expr)
//  } yield RefDatatype(exprType, false)
//  case ValExpr(expr, _) => for {
//    exprType <- trivialTypeof(container, expr)
//    newType <- exprType match {
//      case RefDatatype(datatype, _) => Right(datatype)
//      case datatype => Left(Some(Error.semantic(s"Expected the expression to be invoked to be of a reference type, found expression of type '$datatype'", expr.range)))
//    }
//  } yield newType
//  case IntExpr(_, _) => Right(IntDatatype)
//  case BoolExpr(_, _) => Right(BoolDatatype)
//  case TupleExpr(elements, _) => elements.map(e => trivialTypeof(container, e)).extract.mapBoth(reduceOptionalErrors, datatypes => TupleDatatype(datatypes, false))
//  case BlockExpr(_, expr, _) => trivialTypeof(container, expr)
//  case UnitExpr(_) => Right(UnitDatatype)
//  case DotExpr(expr, iden, _) => ???
//  case FunExpr(parameterPatterns, returnTypeExpr, expr, _) => for {
//    params <- parameterPatterns.map(p => trivialTypeOf(container, p)).extract.mapLeft(reduceOptionalErrors)
//    returnType <- (for {
//      typeExpr <- returnTypeExpr.toRight(None)
//      returnType <- resolveTypeExpr(container, typeExpr).mapLeft(Some.apply)
//    } yield returnType).orElse(trivialTypeof(container, expr))
//  } yield FunDatatype(params, returnType, false)
//  case IfExpr(_, ifBlock, elseBlock, range) => for {
//    ifType <- trivialTypeof(container, ifBlock)
//    elseType <- trivialTypeof(container, elseBlock)
//    result <- (ifType, elseType) match {
//      case (UnitDatatype, UnitDatatype) => Right(UnitDatatype)
//      case (_, UnitDatatype) => Right(ifType)
//      case (UnitDatatype, _) => Right(elseType)
//      case _ if ifType == elseType => Right(ifType)
//      case _ =>
//        val ifBranch = ErrorComponent(ifBlock.range, Some(s"The if branch is of type '$ifType'"))
//        val elseBranch = ErrorComponent(elseBlock.range, Some(s"The else branch is of type '$elseType'"))
//        Left(Some(Error(Error.SEMANTIC, range.file, List(ifBranch, elseBranch), Some(s"Both branches of an if statement must return the same value"))))
//    }
//  } yield result
//}
//
//private def visitIncompletePattern(container: ConstAndTypeContainer, pattern: Pattern[Const], datatype: Datatype): Either[CompilerError, Unit] = pattern match {
//  case pattern@VarPattern(name, typeExpr, _) => typeExpr match {
//    case Some(typeExpr) => for {
//      resolvedDatatype <- resolveTypeExpr(container, typeExpr)
//      result <- if (datatype.isSubtypeOf(resolvedDatatype)) {
//        val const = Const(name, resolvedDatatype)
//        pattern.variable = Some(const)
//        container.addConst(const)
//        Right(())
//      } else {
//        Left(Error.semantic(s"Type mismatch, variable of type $resolvedDatatype does not match value of type $datatype", typeExpr.range))
//      }
//    } yield result
//    case None =>
//      val const = Const(name, datatype)
//      pattern.variable = Some(const)
//      container.addConst(const)
//      Right(())
//  }
//  case TuplePattern(patterns, range) => datatype match {
//    case TupleDatatype(datatypes, _) if patterns.length == datatypes.length => patterns.zip(datatypes).map {
//      case (p, d) => visitIncompletePattern(container, p, d)
//    }.extract.mapBoth(_.reduce(_ | _), _ => ())
//    case _ => Left(Error.semantic("The pattern did not match the return type of the expression", range))
//  }
//}
//
//private def visitCompletePattern(container: ConstAndTypeContainer, pattern: Pattern[Const]): Either[CompilerError, Unit] = pattern match {
//  case pattern@VarPattern(name, typeExpr, _) => for {
//    resolvedDatatype <- resolveTypeExpr(container, typeExpr.get)
//  } yield {
//    val const = Const(name, resolvedDatatype)
//    pattern.variable = Some(const)
//    container.addConst(const)
//    Right(())
//  }
//  case TuplePattern(elements, _) => elements.map(p => visitCompletePattern(container, p)).extract.mapBoth(_.reduce(_ | _), _ => ())
//}
//
//private def resolveConstType(container: ConstAndTypeContainer, constStmt: ConstStmt) = if (constStmt.pattern.incomplete) {
//  for {
//    datatype <- trivialTypeof(container, constStmt.expr).mapLeft(_.getOrElse(Error.semantic("Constants without complete explicit type must be trivially type inferrable (The expression cannot reference other constants, use explicit type annotations where this is not possible)", constStmt.expr.range)))
//    _ <- visitIncompletePattern(container, constStmt.pattern, datatype)
//  } yield ()
//} else {
//  visitCompletePattern(container, constStmt.pattern)
//}
//
//private def resolveConstTypes(container: ConstAndTypeContainer): Either[CompilerError, Unit] = container.constStmts.map(constStmt => resolveConstType(container, constStmt)).extract.mapBoth(_.reduce(_ | _), _ => ())
//
//private def resolveConstValue(container: ConstAndTypeContainer, constStmt: ConstStmt): Either[CompilerError, Unit] = ???
//
//private def resolveConstValues(container: ConstAndTypeContainer): Either[CompilerError, Unit] = container.constStmts.map(constStmt => resolveConstType(container, constStmt)).extract.mapBoth(_.reduce(_ | _), _ => ())
