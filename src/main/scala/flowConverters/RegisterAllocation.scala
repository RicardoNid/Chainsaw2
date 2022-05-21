package org.datenlord
package flowConverters

import scala.collection.mutable


/** An editable table of register allocation
 */
case class RegisterAllocation(timeRange: Int, period: Int) {

  val occupation = mutable.Seq.fill(timeRange)(mutable.Seq[Int]())

  def get(time: Int, reg: Int) = {
    if (reg >= occupation(time).length) -1
    else occupation(time)(reg)
  }

  def alloc(time: Int, reg: Int, value: Int) = {
    if (occupation(time).length > reg) occupation(time)(reg) = value
    else {
      occupation(time) = occupation(time).padTo(reg + 1, -1)
      occupation(time)(reg) = value
    }
  }

  def set(time: Int, reg: Int, value: Int) = {
    val targets = (time until timeRange).filter(t => (t - time) % period == 0)
    targets.foreach(target => alloc(target, reg, value))
  }

  def registerCount = occupation.map(_.length).max

  val charChange = (index: Int) => if (index < 0) "-" else index.toString

  override def toString = {
    val head = "register allocation: \n"
    val content = occupation.map(_.map(charChange(_).padTo(2, ' ')).mkString(" "))
      .zipWithIndex.map { case (str, time) => s"time: $time".padTo(10, ' ') + s"| $str |" }.mkString("\n")
    head + content
  }
}
