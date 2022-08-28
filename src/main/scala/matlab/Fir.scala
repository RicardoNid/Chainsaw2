package org.datenlord
package matlab

object FilterType extends Enumeration {
  val low, bandpass, high, stop = Value
  type FilterType = Value
}

import matlab.FilterType._



case class Fir(coeffs: Seq[Double]) {

  def filter(dataIn:Array[Double]) = MatlabFeval[Array[Double]]("conv", 0, dataIn, coeffs)

}

object Fir {

  def getWindow(order: Int, window: String) = MatlabFeval[Array[Double]](window, 0, order + 1)

  /** matlab fir1 function, Window-based FIR filter design
   *
   * @param order  order of fir filter, the output filter has (order+1) taps
   * @param freqConstraint
   *               1.scalar -> lowpass or highpass filter
   *               2.twe-element, increasing order -> bandpass or bandstop filter
   *               3.multi-element, decreasing order -> multiband filter
   * @param windowType window specified by a vector of (order+1) elements, hamming window by default
   * @return coefficients of the fir filter
   */
  def getWindowedFir(order: Int, freqConstraint: Array[Double], ftType: FilterType, windowType: String = "hamming") = {
    val window = getWindow(order, windowType)
    val coeffs = MatlabFeval[Array[Double]]("fir1", 0, order, freqConstraint, ftType.toString, window)
    Fir(coeffs)
  }

  def getResampleFir(p: Int, q: Int) = {
    val coeffs = MatlabFeval[Array[Double]]("resample", 1, Array(0.0), p, q)
    Fir(coeffs)
  }

  def main(args: Array[String]): Unit = {
    //    println(getResampleFir(4, 1))
    println(getWindowedFir(50, Array(0.1, 0.4), low).coeffs.mkString(" "))

  }
}
