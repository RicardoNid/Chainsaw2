package org.datenlord
package dataFlow

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

object Utils {
  def switch22(a: Bits, b: Bits, control: Bool) = {
    val retA = Mux(control, b, a)
    val retB = Mux(control, a, b)
    (retA, retB)
  }

  def DSD(K: Int, a: Bits, b: Bits, control: Bool) = {
    val (sA, sB) = switch22(a, b.d(K), control)
    (sA.d(K), sB)
  }

  def SEU(K: Int, data: Bits, control: Bool) = {
    val afterM0, afterM1 = cloneOf(data)
    afterM0 := Mux(control, afterM0.d(K), data)
    afterM1 := Mux(control, data, afterM0.d(K))
    afterM1
  }
}
