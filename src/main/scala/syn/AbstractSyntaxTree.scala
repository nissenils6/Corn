package syn

import core.*
import lex.*
import sem.*

import scala.collection.mutable

abstract class AnyVar {
  def name: String
  var datatype: Option[Datatype]
}

class Var(val name: String, val stmt: Option[VarStmt]) extends AnyVar {
  var datatype: Option[Datatype] = None
}

class Const(val name: String, val stmt: Option[ConstStmt]) extends AnyVar {
  var datatype: Option[Datatype] = None
  var value: Option[ConstVal] = None
}

class TypeVar(val name: String, val stmt: Option[TypeStmt]) {
  var value: Option[Datatype] = None
}

trait Fun {
  def signature: Option[FunDatatype]
}

abstract class Expr {
  def range: FilePosRange
  var parent: Option[Container] = None

  def format(indentation: Int): String = this match {
    case CallExpr(fun, args, _) => s"${fun.format(indentation)}(${args.map(_.format(indentation)).mkString(", ")})"
    case IdenExpr(iden, _) => iden
    case RefExpr(expr, _) => s"@${expr.format(indentation)}"
    case ValExpr(expr, _) => s"!${expr.format(indentation)}"
    case IntExpr(int, _) => int.toString
    case BoolExpr(bool, _) => bool.toString
    case TupleExpr(elements, _) => s"(${elements.map(_.format(indentation)).mkString(", ")})"
    case blockExpr@BlockExpr(stmts, expr, _) => s"{\n${blockExpr.formatConsts(indentation)}\n${blockExpr.formatTypes(indentation)}\n${stmts.map(_.format(indentation + 1)).mkString}${" " * (indentation + 1)}${expr.format(indentation + 1)}\n${" " * indentation}}"
    case UnitExpr(_) => "()"
    case DotExpr(expr, iden, _) => s"${expr.format(indentation)}.$iden"
    case FunExpr(parameters, returnType, expr, _) => s"(${parameters.map(_.format(indentation)).mkString(", ")})${returnType.map(": " + _).getOrElse("")} => ${expr.format(indentation)}"
    case IfExpr(condition, ifBlock, elseBlock, _) => s"if ${condition.format(indentation)} then ${ifBlock.format(indentation)} else ${elseBlock.format(indentation)}"
  }
}

case class CallExpr(function: Expr, args: List[Expr], range: FilePosRange) extends Expr

case class IdenExpr(iden: String, range: FilePosRange) extends Expr {
  var variable: Option[AnyVar] = None
}

case class RefExpr(expr: Expr, range: FilePosRange) extends Expr
case class ValExpr(expr: Expr, range: FilePosRange) extends Expr
case class IntExpr(int: Long, range: FilePosRange) extends Expr
case class BoolExpr(bool: Boolean, range: FilePosRange) extends Expr
case class TupleExpr(elements: List[Expr], range: FilePosRange) extends Expr

case class BlockExpr(stmts: List[Stmt], expr: Expr, range: FilePosRange) extends Expr with Container {
  var module: Option[Module] = None

  val vars: mutable.Map[String, Var] = mutable.Map.empty
  val consts: mutable.Map[String, List[Const]] = mutable.Map.empty
  val types: mutable.Map[String, TypeVar] = mutable.Map.empty

  val regStmts: List[Stmt] = stmts.filter(s => !s.isInstanceOf[LocalConstStmt] && !s.isInstanceOf[LocalTypeStmt])
  val constStmts: List[LocalConstStmt] = stmts.filter(_.isInstanceOf[LocalConstStmt]).asInstanceOf[List[LocalConstStmt]]
  val typeStmts: List[LocalTypeStmt] = stmts.filter(_.isInstanceOf[LocalTypeStmt]).asInstanceOf[List[LocalTypeStmt]]
}

case class UnitExpr(range: FilePosRange) extends Expr
case class DotExpr(expr: Expr, iden: String, range: FilePosRange) extends Expr

case class FunExpr(parameterPatterns: List[Pattern[Var]], returnTypeExpr: Option[TypeExpr], expr: Expr, range: FilePosRange) extends Expr with Fun {
  var optFun: Option[opt.Fun] = None
  var signature: Option[FunDatatype] = None
  var bodyTypeChecked: Boolean = false
}

case class BuiltinFun(parameters: List[Datatype], returnType: Datatype, eval: Option[List[ConstVal] => ConstVal], optFun: Option[opt.Fun]) extends Fun {
  val signature: Option[FunDatatype] = Some(FunDatatype(parameters, returnType, false))
}

case class IfExpr(condition: Expr, ifBlock: Expr, elseBlock: Expr, range: FilePosRange) extends Expr

trait AnyVarStmt {
  def range: FilePosRange
  def expr: Expr
  def nameRange: FilePosRange
}

trait VarStmt extends AnyVarStmt {
  def parent: Option[Container]
  def pattern: Pattern[Var]
}

trait ConstStmt extends AnyVarStmt {
  def parent: Option[Container]
  def pattern: Pattern[Const]
  var bodyTypeChecked: Boolean
  var value: Option[ConstVal]
}

trait TypeStmt {
  def range: FilePosRange
  def name: String
  def typeExpr: TypeExpr
  def nameRange: FilePosRange
  def typeVar: TypeVar
}

trait Container {
  def module: Option[Module]
  def parent: Option[Container]

  def consts: mutable.Map[String, List[Const]]
  def types: mutable.Map[String, TypeVar]

  def constStmts: List[ConstStmt]
  def typeStmts: List[TypeStmt]

  def addConst(const: Const): Unit = if (consts.contains(const.name)) {
    consts(const.name) = const :: consts(const.name)
  } else {
    consts(const.name) = List(const)
  }

  def addType(typeVar: TypeVar): Either[CompilerError, Unit] = types.get(typeVar.name) match {
    case Some(prior) =>
      assert(typeVar.stmt.nonEmpty, "Builtin type variable with cannot have duplicate name")
      val Some(typeStmt) = typeVar.stmt
      val newDefinition = ErrorComponent(typeStmt.nameRange, Some(s"new definition of type '${typeVar.name}'"))
      if (prior.stmt.nonEmpty) {
        val definedHere = ErrorComponent(prior.stmt.get.nameRange, Some(s"but '${prior.name}' is already defined here"))
        Left(Error(Error.SEMANTIC, typeStmt.nameRange.file, List(newDefinition, definedHere), Some(s"multiple type definitions with the same name '${typeVar.name}'")))
      } else {
        Left(Error(Error.SEMANTIC, typeStmt.nameRange.file, List(newDefinition), Some(s"type definition with same name as builtin type '${typeVar.name}'")))
      }
    case None =>
      types(typeVar.name) = typeVar
      Right(())
  }

  def lookupType(name: String, nameRange: FilePosRange): Either[CompilerError, TypeVar] = types.get(name) match {
    case Some(typeVar) => Right(typeVar)
    case None => parent.map(_.lookupType(name, nameRange)) match {
      case Some(result) => result
      case None => Left(Error.semantic(s"Could not resolve '$name' as a type", nameRange))
    }
  }

  def formatConsts(indentation: Int): String = (for {
    constList <- consts.values
    const <- constList
  } yield s"${" " * (indentation + 1)}${const.name} : ${const.datatype.map(_.toString).getOrElse("???")} : ${const.value.map(_.toString).getOrElse("???")}\n").mkString(s"${" " * indentation}consts {\n", "", s"${" " * indentation}}\n")

  def formatTypes(indentation: Int): String = (for {
    typeVar <- types.values
  } yield s"${" " * (indentation + 1)}${typeVar.name} = ${typeVar.value.map(_.toString).getOrElse("???")}\n").mkString(s"${" " * indentation}consts {\n", "", s"${" " * indentation}}\n")
}

abstract class Stmt {
  def format(indentation: Int): String = " " * indentation + (this match {
    case ExprStmt(expr, _) => expr.format(indentation)
    case AssignVarStmt(iden, expr, _) => s"$iden = ${expr.format(indentation)}"
    case AssignRefStmt(refExpr, expr, _) => s"!${refExpr.format(indentation)} = ${expr.format(indentation)}"
    case LocalVarStmt(pattern, expr, _, _) => s"${pattern.format(indentation)} = ${expr.format(indentation)}"
    case LocalConstStmt(pattern, expr, _, _) => s"${pattern.format(indentation)} : ${expr.format(indentation)}"
  }) + ";\n"
}

case class ExprStmt(expr: Expr, range: FilePosRange) extends Stmt

case class AssignVarStmt(iden: String, expr: Expr, range: FilePosRange) extends Stmt {
  var variable: Option[Var] = None
}

case class AssignRefStmt(refExpr: Expr, expr: Expr, range: FilePosRange) extends Stmt

case class LocalVarStmt(pattern: Pattern[Var], expr: Expr, nameRange: FilePosRange, range: FilePosRange) extends Stmt with VarStmt {
  var parent: Option[Container] = None
}

case class LocalConstStmt(pattern: Pattern[Const], expr: Expr, nameRange: FilePosRange, range: FilePosRange) extends Stmt with ConstStmt {
  var parent: Option[Container] = None
  var bodyTypeChecked: Boolean = false
  var value: Option[ConstVal] = None
}

case class LocalTypeStmt(name: String, typeExpr: TypeExpr, nameRange: FilePosRange, range: FilePosRange) extends Stmt with TypeStmt {
  val typeVar: TypeVar = TypeVar(name, Some(this))
}

abstract class TypeExpr {
  def range: FilePosRange

  override def toString: String = this match {
    case UnitTypeExpr(_) => "()"
    case IdenTypeExpr(iden, _) => iden
    case TupleTypeExpr(elements, _) => s"(${elements.mkString(", ")})"
    case MutTypeExpr(typeExpr, _) => s"mut $typeExpr"
    case RefTypeExpr(typeExpr, _) => s"@$typeExpr"
    case FunTypeExpr(params, returnType, _) => s"(${params.mkString(", ")}) => $returnType"
  }
}

case class UnitTypeExpr(range: FilePosRange) extends TypeExpr
case class IdenTypeExpr(iden: String, range: FilePosRange) extends TypeExpr
case class TupleTypeExpr(elements: List[TypeExpr], range: FilePosRange) extends TypeExpr
case class MutTypeExpr(typeExpr: TypeExpr, range: FilePosRange) extends TypeExpr
case class RefTypeExpr(typeExpr: TypeExpr, range: FilePosRange) extends TypeExpr
case class FunTypeExpr(params: List[TypeExpr], returnType: TypeExpr, range: FilePosRange) extends TypeExpr

abstract class Pattern[T <: AnyVar] {
  def range: FilePosRange

  def format(indentation: Int): String = this match {
    case VarPattern(name, typeExpr, _) => s"$name : ${typeExpr.map(_.toString).getOrElse("auto")}"
    case TuplePattern(elements, _) => s"(${elements.map(_.format(indentation)).mkString(", ")})"
  }

  def incomplete: Boolean = this match {
    case VarPattern(_, typeExpr, _) => typeExpr.isEmpty
    case TuplePattern(elements, _) => elements.exists(_.incomplete)
  }

  def foreach[U](f: VarPattern[T] => U): Unit = this match {
    case pattern: VarPattern[T] => f(pattern)
    case TuplePattern(elements, _) => elements.foreach(_.foreach(f))
  }

  def foreachError(f: VarPattern[T] => Either[CompilerError, Unit]): Either[CompilerError, Unit] = this match {
    case pattern: VarPattern[T] => f(pattern)
    case TuplePattern(elements, _) => elements.map(_.foreachError(f)).extract.mapBoth(_.reduce(_ | _), _ => ())
  }
}

case class VarPattern[T <: AnyVar](name: String, typeExpr: Option[TypeExpr], range: FilePosRange) extends Pattern[T] {
  var variable: Option[T] = None
}

case class TuplePattern[T <: AnyVar](elements: List[Pattern[T]], range: FilePosRange) extends Pattern[T]

abstract class GlobalStmt {
  def range: FilePosRange

  def format(indentation: Int): String = this match {
    case GlobalVarStmt(pattern, expr, _, _) => s"${" " * indentation}${pattern.format(indentation)} = ${expr.format(indentation)}\n"
    case GlobalConstStmt(pattern, expr, _, _) => s"${" " * indentation}${pattern.format(indentation)} : ${expr.format(indentation)}\n"
    case TypeGlobalStmt(name, typeExpr, _, _) => s"${" " * indentation}type $name = $typeExpr\n"
  }
}

case class GlobalVarStmt(pattern: Pattern[Var], expr: Expr, nameRange: FilePosRange, range: FilePosRange) extends GlobalStmt with VarStmt {
  var parent: Option[Container] = None
  var bodyTypeChecked: Boolean = false
}

case class GlobalConstStmt(pattern: Pattern[Const], expr: Expr, nameRange: FilePosRange, range: FilePosRange) extends GlobalStmt with ConstStmt {
  var parent: Option[Container] = None
  var bodyTypeChecked: Boolean = false
  var value: Option[ConstVal] = None
}

case class TypeGlobalStmt(name: String, typeExpr: TypeExpr, nameRange: FilePosRange, range: FilePosRange) extends GlobalStmt with TypeStmt {
  val typeVar: TypeVar = TypeVar(name, Some(this))
}

case class Module(globalStmts: List[GlobalStmt], file: File) extends Container {
  val module: Option[Module] = Some(this)
  val parent: Option[Container] = None

  val vars: mutable.Map[String, List[Var]] = mutable.Map.empty
  val consts: mutable.Map[String, List[Const]] = mutable.Map.empty
  val types: mutable.Map[String, TypeVar] = mutable.Map.empty

  val varStmts: List[GlobalVarStmt] = globalStmts.filter(_.isInstanceOf[GlobalVarStmt]).asInstanceOf[List[GlobalVarStmt]]
  val constStmts: List[GlobalConstStmt] = globalStmts.filter(_.isInstanceOf[GlobalConstStmt]).asInstanceOf[List[GlobalConstStmt]]
  val typeStmts: List[TypeGlobalStmt] = globalStmts.filter(_.isInstanceOf[TypeGlobalStmt]).asInstanceOf[List[TypeGlobalStmt]]

  def addVar(variable: Var): Unit = vars.updateWith(variable.name) {
    case Some(list) => Some(variable :: list)
    case None => Some(List(variable))
  }

  def formatGlobalStmts: String = globalStmts.map(_.format(1)).mkString("\n")

  def format: String = s"module ${file.name} {\n${formatConsts(1)}\n${formatTypes(1)}\n$formatGlobalStmts\n}"
}
