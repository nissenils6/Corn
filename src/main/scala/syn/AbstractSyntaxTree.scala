package syn

import core.*
import lex.*
import sem.*

import scala.collection.mutable

abstract class AnyVar {
  def name: String
  def datatype: Datatype
}

abstract class Var extends AnyVar {

}

case class GlobalVar(name: String, datatype: Datatype) extends Var {

}

case class LocalVar(name: String, datatype: Datatype) extends Var {

}

abstract class Const extends AnyVar {
  var value: Option[ConstVal] = None
}

case class GlobalConst(name: String, datatype: Datatype) extends Const {

}

case class LocalConst(name: String, datatype: Datatype) extends Const {

}

case class TypeVar(name: String) {
  var value: Option[Datatype] = None
}

trait Fun {
  def signature: Option[FunDatatype]
}

abstract class Expr {
  def range: FilePosRange

  def format(indentation: Int): String = this match {
    case CallExpr(fun, List(a, b), _) => s"(${a.format(indentation)} ${fun.format(indentation)} ${b.format(indentation)})"
    case CallExpr(fun, args, _) => s"${fun.format(indentation)}(${args.map(_.format(indentation)).mkString(", ")})"
    case IdenExpr(iden, _) => iden
    case RefExpr(expr, _) => s"@${expr.format(indentation)}"
    case ValExpr(expr, _) => s"!${expr.format(indentation)}"
    case IntExpr(int, _) => int.toString
    case BoolExpr(bool, _) => bool.toString
    case TupleExpr(elements, _) => s"(${elements.map(_.format(indentation)).mkString(", ")})"
    case BlockExpr(stmts, expr, _) => s"{\n${stmts.map(_.format(indentation + 1)).mkString}${" " * (indentation + 1)}${expr.format(indentation + 1)}\n${" " * indentation}}"
    case UnitExpr(_) => "()"
    case DotExpr(expr, iden, _) => s"${expr.format(indentation)}.$iden"
    case FunExpr(parameters, returnType, expr, _) => s"(${parameters.map(_.format(indentation)).mkString(", ")})${returnType.map(": " + _).getOrElse("")} => ${expr.format(indentation)}"
    case BuiltinFunExpr(parameterPatterns, returnTypeExpr, _, _, _) => s"(${parameterPatterns.mkString(", ")}): $returnTypeExpr => <builtin>"
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

case class BlockExpr(stmts: List[Stmt], expr: Expr, range: FilePosRange) extends Expr {
  var vars: mutable.Map[String, LocalVar] = mutable.Map.empty
  var consts: mutable.Map[String, LocalConst] = mutable.Map.empty
  var types: mutable.Map[String, TypeVar] = mutable.Map.empty
}

case class UnitExpr(range: FilePosRange) extends Expr
case class DotExpr(expr: Expr, iden: String, range: FilePosRange) extends Expr

case class FunExpr(parameterPatterns: List[Pattern[LocalVar]], returnTypeExpr: Option[TypeExpr], expr: Expr, range: FilePosRange) extends Expr with Fun {
  var optFun: Option[opt.Fun] = None
  var signature: Option[FunDatatype] = None
}

case class BuiltinFunExpr(parameters: List[Datatype], returnType: Datatype, eval: Option[List[ConstVal] => ConstVal], optFun: Option[opt.Fun], range: FilePosRange) extends Expr with Fun {
  val signature: Option[FunDatatype] = Some(FunDatatype(parameters, returnType, false))
}

case class IfExpr(condition: Expr, ifBlock: Expr, elseBlock: Expr, range: FilePosRange) extends Expr

abstract class Stmt {
  def format(indentation: Int): String = " " * indentation + (this match {
    case ExprStmt(expr, _) => expr.format(indentation)
    case AssignVarStmt(iden, expr, _) => s"$iden = ${expr.format(indentation)}"
    case AssignRefStmt(refExpr, expr, _) => s"!${refExpr.format(indentation)} = ${expr.format(indentation)}"
    case VarStmt(pattern, expr, _) => s"${pattern.format(indentation)} = ${expr.format(indentation)}"
    case ConstStmt(pattern, expr, _) => s"${pattern.format(indentation)} : ${expr.format(indentation)}"
  }) + ";\n"
}

case class ExprStmt(expr: Expr, range: FilePosRange) extends Stmt
case class AssignVarStmt(iden: String, expr: Expr, range: FilePosRange) extends Stmt
case class AssignRefStmt(refExpr: Expr, expr: Expr, range: FilePosRange) extends Stmt
case class VarStmt(pattern: Pattern[LocalVar], expr: Expr, range: FilePosRange) extends Stmt

case class ConstStmt(pattern: Pattern[LocalConst], expr: Expr, range: FilePosRange) extends Stmt {
  var value: Option[ConstVal] = None
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
    case VarPattern(name, datatype, _) => s"$name${datatype.map(": " + _).getOrElse("")}"
    case TuplePattern(elements, _) => s"(${elements.map(_.format(indentation)).mkString(", ")})"
  }
}

case class VarPattern[T <: AnyVar](name: String, datatype: Option[TypeExpr], range: FilePosRange) extends Pattern[T] {
  var variable: Option[T] = None
}

case class TuplePattern[T <: AnyVar](elements: List[Pattern[T]], range: FilePosRange) extends Pattern[T]

abstract class GlobalStmt {
  override def toString: String = this match {
    case VarGlobalStmt(pattern, expr, _) => s"${pattern.format(0)} = ${expr.format(0)}"
    case TypeGlobalStmt(name, typeExpr) => s"type $name = $typeExpr"
  }
}

case class VarGlobalStmt(pattern: Pattern[GlobalVar], expr: Expr, range: FilePosRange) extends GlobalStmt {

}

case class ConstGlobalStmt(pattern: Pattern[GlobalConst], expr: Expr, range: FilePosRange) extends GlobalStmt {
  var value: Option[ConstVal] = None
}

case class TypeGlobalStmt(name: String, typeExpr: TypeExpr) extends GlobalStmt {
  val typeVar: TypeVar = TypeVar(name)
}

case class Module(globalStmts: List[GlobalStmt], file: File) {
  var vars: mutable.Map[String, GlobalVar] = mutable.Map.empty
  var consts: mutable.Map[String, GlobalConst] = mutable.Map.empty
  var types: mutable.Map[String, TypeVar] = mutable.Map.empty
}
