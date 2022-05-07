package org.datenlord
package ip.fft

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

object BaseDft {

  def radixRDft(radix: Int) = {
    (dataIn: Seq[ComplexFix]) =>
      radix match {
        case 2 => Seq(dataIn(0) +^ dataIn(1), dataIn(0) -^ dataIn(1))
      }
  }

  def latency(radix:Int) = {
    radix match {
      case 2 => 0
    }
  }
}
