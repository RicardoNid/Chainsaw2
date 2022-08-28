package org.datenlord
package examples

import com.mathworks.engine._

import scala.io.StdIn
import scala.util.Random

object UsingMatlab extends App {
  // initialize a Matlab engine
  val eng = MatlabEngine.startMatlab()

  // using eval to execute your command, no return,
  // this function works by making side effects in Matlab workspace, and this will be shown in terminal
  eng.eval("1+2")

  // using feval to execute a function and get return value
  // the type of return value can be very tricky, as Matlab doesn't have a strict type system
  //  val ret: Int = eng.feval("sum", Array(1,2)) // wrong, as the return value is Double
  val ret: Double = eng.feval("sum", Array(1, 2)) // wrong, as the return value is Double
  println(ret)

  // put/get variable to/from Matlab workspace
  // again, pay attention to the return type
  eng.putVariable("x", 1)
  val x: Int = eng.getVariable("x")
  println(x)

  // actually, Matlab APIs are JAVA APIs, so they expect Array, rather than Seq
  // also, as Matlab APIs generally have Object type in their signatures, implicit collection conversion won't help, you have to use Arrays explicitly

  import collection.JavaConversions._
  //  val another: Double = eng.feval("sum", Seq(1, 2)) // won't work
  val another: Double = eng.feval("sum", Array(1, 2)) // work

  // besides type system, another tricky part of Matlab APIs is element order in Matrix, different from most high-level languages, Matlab is column-major, rather than row-major
  // but still, Matlab view rows as the first dimension, so you can easily put and get a 2-dimensional matrix without conversion
  eng.putVariable("y", Array(Array(1,2), Array(3,4)))
  val y: Array[Array[Int]] = eng.getVariable("y")
  eng.eval("y")
  println(y.map(_.mkString(" ")).mkString("\n"))

  // you can plot in scala, and view it in a pop-up
  eng.eval("plot([1,2], [3,4])")
  // but you should make your application wait for the input, or it will disappear in an instant
  // obviously, readLine can be used as a breakpoint
  val line = StdIn.readLine()

  // Matlab can be a good reference model for digital signal processing
  val time = (0 until 128).map(_ => Random.nextDouble()).toArray
  // Matlab has its complex type, but it has nearly no methods, so an implicit class will help, we leave this in the top package
  import com.mathworks.matlab.types._ // import types
  val freq = eng.feval("fft", time).asInstanceOf[Array[Complex]]
  println(freq.mkString(" "))

  // generally, because of the type system conflicts(scala-static vs matlab-dynamic), using eval function with literal command may be a better choice
  // literal command work for consistently for Int,Double and Complex, while type system may fail
  eng.eval(s"y = fft(${time.toMatlabLiteral})")
  println(eng.getVariable("y").asInstanceOf[Seq[Double]])

}
