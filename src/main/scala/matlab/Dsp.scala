package org.datenlord
package matlab

object Dsp {

  def upfirdn(data: Array[Double], coeffs: Array[Double], p: Int, q: Int) =
    MatlabFeval[Array[Double]]("upfirdn", 0, data, coeffs, p.toDouble, q.toDouble)

  def fir(data: Array[Double], coeffs: Array[Double]) = {
    MatlabFeval[Array[Double]]("conv", 0, data, coeffs)
  }
}
