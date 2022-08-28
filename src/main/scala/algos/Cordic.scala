package org.datenlord
package algos

import breeze.numerics._
import breeze.numerics.constants._

object Cordic {

  def approaching(angle: Double, epsilon: Double = 1e-4) = {

    var err = Double.MaxValue
    var i = 0
    var accumulated = 0.0

    while (abs(err) > epsilon && i < 100){
      val step = atan(pow(2.0, -i))
      if(accumulated < angle) accumulated += step
      else accumulated -= step
      i += 1
      err = angle - accumulated
    }

    logger.info(s"err = $err by $i steps")
  }

  def main(args: Array[String]): Unit = {
    approaching(-0.125 * Pi, 1e-12)
  }

}
