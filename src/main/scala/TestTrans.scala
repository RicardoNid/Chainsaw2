package org.datenlord

import breeze.math._

abstract class TestTrans {

  def transform(dataIn: Seq[_]): Seq[_] = dataIn

}


case class double2Complex() extends TestTrans {

   override def transform(dataIn: Seq[_]) = dataIn.map(data => Complex(data.asInstanceOf[Double], 0))

}

case class complex2Double() extends TestTrans {

  override def transform(dataIn: Seq[_]) = dataIn.map(_.asInstanceOf[Complex].real)

}

object TestTrans {
  def main(args: Array[String]): Unit = {
    val temp = Seq(double2Complex(), complex2Double())
    val data = Seq(1.0,1.0)
    println(temp(1).transform(temp(0).transform(data)).mkString(" "))
    val transform = temp(1).transform _
    transform match {
      // can't find out because of type erasure
      case d2c: (Seq[Double] => Seq[Complex]) => println("d2c")
      case c2d: (Seq[Complex] => Seq[Double]) => println("c2d")
    }
  }
}