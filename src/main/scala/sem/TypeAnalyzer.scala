package sem

import core.*
import syn.*

private def resolveTypeExprRecursive(container: Container, typeExpr: TypeExpr, visited: List[(TypeStmt, FilePosRange)]): Either[CompilerError, Datatype] = typeExpr match {
  case UnitTypeExpr(_) => Right(UnitDatatype)
  case IdenTypeExpr(iden, range) => for {
    typeVar <- container.lookupType(iden, range)
    datatype <- resolveTypeVar(container, typeVar, range, visited)
  } yield datatype
  case TupleTypeExpr(elements, _) => for {
    datatypes <- elements.map(typeExpr => resolveTypeExprRecursive(container, typeExpr, visited)).extract.mapLeft(_.reduce(_ | _))
  } yield TupleDatatype(datatypes, false)
  case MutTypeExpr(typeExpr, range) => ???
  case RefTypeExpr(typeExpr, range) => ???
  case FunTypeExpr(params, returnType, range) => ???
}

private def resolveTypeVar(container: Container, typeVar: TypeVar, causeRange: FilePosRange, visited: List[(TypeStmt, FilePosRange)]): Either[CompilerError, Datatype] = typeVar.value match {
  case Some(datatype) => Right(datatype)
  case None =>
    val typeStmt = typeVar.stmt.get
    val index = visited.indexWhere(_._1 eq typeStmt)
    if (index >= 0) {
      val chain = ((typeStmt, causeRange) :: visited).take(index + 2).reverse
      val components = ErrorComponent(chain.head._2, Some(s"type '${chain.head._1.name}' is to be evaluated")) ::
        chain.tail.zip(chain).map { case ((cur, range), (cause, _)) =>
          ErrorComponent(range, Some(s"In order to evaluate type '${cause.name}', type '${cur.name}' must be evaluated"))
        }
      Left(Error(Error.SEMANTIC, causeRange.file, components, Some(s"Cycle detected when trying to evaluate type '${typeStmt.name}'")))
    } else {
      for {
        datatype <- resolveTypeExprRecursive(container, typeStmt.typeExpr, (typeStmt, causeRange) :: visited)
      } yield {
        typeVar.value = Some(datatype)
        datatype
      }
    }
}

private def resolveTypes(container: Container): Either[CompilerError, Unit] = for {
  _ <- container.typeStmts.map { typeStmt =>
    container.addType(typeStmt.typeVar)
  }.extract.mapBoth(_.reduce(_ | _), _ => ())
  _ <- container.typeStmts.map { typeStmt =>
    resolveTypeVar(container, typeStmt.typeVar, typeStmt.nameRange, List())
  }.extract.mapBoth(_.reduce(_ | _), _ => ())
} yield ()

private def resolveTypeExpr(container: Container, typeExpr: TypeExpr): Either[CompilerError, Datatype] = typeExpr match {
  case UnitTypeExpr(_) => Right(UnitDatatype)
  case IdenTypeExpr(iden, range) => container.types.get(iden).map(_.value.get).toRight(Error.semantic(s"Could not resolve '$iden' as a type", range))
  case TupleTypeExpr(elements, _) => for {
    datatypes <- elements.map(e => resolveTypeExpr(container, e)).extract.mapLeft(_.reduce(_ | _))
  } yield TupleDatatype(datatypes, false)
  case MutTypeExpr(typeExpr, range) => ???
  case RefTypeExpr(typeExpr, range) => ???
  case FunTypeExpr(params, returnType, range) => ???
}
