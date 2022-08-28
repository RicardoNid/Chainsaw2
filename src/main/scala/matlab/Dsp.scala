package org.datenlord
package matlab

object Dsp {

  def upfirdn(data: Array[Double], coeff: Array[Double], p: Int, q: Int) =
    MatlabFeval[Array[Double]]("upfirdn", 0, data, coeff, p.toDouble, q.toDouble)
}
