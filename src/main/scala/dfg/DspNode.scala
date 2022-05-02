package org.datenlord
package dfg

abstract class DspNode[T] {

  val exeTime: Double
  val delay: Int
  val period: Int
  val impl: Array[T] => Array[T]

}

