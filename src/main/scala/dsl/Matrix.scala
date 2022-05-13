package org.datenlord
package dsl
import scala.reflect.ClassTag

case class Reuse(spaceReuse: Int, timeReuse: Int, fold: Int, iterationLatency:Int){
  override def toString = s"reuse: space = $spaceReuse, time = $timeReuse, fold = $fold"
}
