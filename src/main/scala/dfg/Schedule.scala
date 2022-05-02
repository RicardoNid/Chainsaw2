package org.datenlord
package dfg

case class Schedule(time: Array[Int], period: Int) {

  def getTimeExpanded(newPeriod:Int) =
    (0 until newPeriod / period).flatMap(i => time.map(j => i * period + j)).toArray

  def +(that:Schedule) = {
    val newPeriod = lcm(this.period, that.period)
    val newTime = this.getTimeExpanded(newPeriod) ++ that.getTimeExpanded(newPeriod)
    Schedule(newTime, newPeriod)
  }

  override def toString = s"(${time.mkString(", ")}) \\over $period"
}

object Schedule {

  def main(args: Array[String]): Unit = {
    println(Schedule(Array(0,1),3))
    println(Schedule(Array(0,1), 3) + Schedule(Array(4), 6))
  }

}
