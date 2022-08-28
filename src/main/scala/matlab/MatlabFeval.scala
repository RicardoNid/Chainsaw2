package org.datenlord
package matlab

object MatlabFeval {
  def apply[T](funcName: String, retId: Int, params: Any*) = {
    val retList = (0 until retId + 1).map(i => s"y$i").toArray.toMatlabLiteral
    val paramList = (0 until params.length).map(i => s"x$i")
    params.zip(paramList).foreach { case (param, name) => matlabEngine.putVariableAsync(name, param) }
    val command = s"$retList = $funcName(${paramList.mkString(", ")});"
    logger.info(s"executing $command")
    matlabEngine.evalAsync(command)
    matlabEngine.getVariableAsync(s"y$retId").get().asInstanceOf[T]
  }
}
