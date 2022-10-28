import scala.collection.mutable

class CodeGen {
  private val globalVars = mutable.StringBuilder()
  private val funs = mutable.Buffer[AsmBuilder]()
  private val initCode = AsmBuilder(None)
  private var globalVarCount = 0
  def createGlobalVar(size: Int): Int = {
    val varId = globalVarCount
    globalVars.append(s"V$varId:\n  alloc $size")
    globalVarCount += 1
    varId
  }

  def createFun(): AsmBuilder = {
    val funId = funs.length
    val fun = AsmBuilder(Some(funId))
    funs.append(fun)
    fun
  }

  def initFun: AsmBuilder = initCode
}

class AsmBuilder(funId: Option[Int]) {
  private val stringBuilder = mutable.StringBuilder(s"${funId.map(id => s"F$id").getOrElse("_start")}:\n")

  def line(string: String): AsmBuilder = {
    stringBuilder.append(s"  $string\n")
    this
  }
}
