package org
import org.datenlord.xilinx._
import org.slf4j.LoggerFactory
import spinal.core.Component

package object datenlord {

  val logger = LoggerFactory.getLogger("datenlord logger")

  def VivadoImpl[T <: Component](gen: => T, name: String = "temp", xdcPath: String = null) = {
    val report = VivadoFlow(design = gen, taskType = IMPL, topModuleName = name, workspacePath = s"./$name").doFlow()
    report.printArea()
    report.printFMax()
    report
  }

}
