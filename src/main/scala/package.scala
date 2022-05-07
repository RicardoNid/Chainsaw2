package org

import org.datenlord.xilinx._
import org.slf4j.LoggerFactory
import spinal.core._
import spinal.lib.Delay

import scala.reflect.ClassTag
import scala.util.Random
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

package object datenlord {

  val logger = LoggerFactory.getLogger("datenlord logger")

  def VivadoImpl[T <: Component](gen: => T, name: String = "temp", xdcPath: String = null) = {
    val report = VivadoFlow(design = gen, taskType = IMPL, topModuleName = name, workspacePath = s"./$name").doFlow()
    report.printArea()
    report.printFMax()
    report
  }

  implicit class DataUtil[T <: Data](data: T) {
    def d(cycle: Int): T = Delay(data, cycle)

    def validAfter(cycle: Int) = Delay(data, cycle, init = False).asInstanceOf[Bool]
  }

  implicit class arrayUtil[T: ClassTag](array: Array[T]) {
    def divide(group: Int) = array.grouped(array.length / group).toArray

    def prevAndNext(f: ((T, T)) => Unit) = array.init.zip(array.tail).foreach(f)
  }

  implicit class flowFragmentUtil[T <: Data](flow: Flow[Fragment[T]]) {

  }

  def nextBigInt(width: Int) = BigInt(Random.nextString(width).map(_ % 2).mkString(""), 2)

  def logR(n: Int, radix: Int) = {
    var current = n
    var ret = 0
    while (current > 1) {
      require(current % radix == 0)
      current /= radix
      ret += 1
    }
    ret
  }

  def powR(radix: Int, exp: Int) = Seq.fill(exp)(radix).product

}
