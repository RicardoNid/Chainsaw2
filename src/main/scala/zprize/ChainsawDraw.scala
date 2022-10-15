package org.datenlord
package zprize

import breeze.linalg.{DenseVector, max}
import breeze.math.Complex
import breeze.numerics.abs
import breeze.stats.mean
import matlab._

import scala.reflect.ClassTag

object ChainsawDraw {

  def draw[T: ClassTag](yours: Seq[T], golden: Seq[T], plotLine: Seq[String], name: String): Unit = {
    logger.info(s"generating the figure yours vs golden...")

    def convert(data: Seq[T]) = data.head match {
      case _: Complex => data.map { b =>
        val complex = b.asInstanceOf[Complex]
        new MComplex(complex.real, complex.imag)
      }.toArray
      case _ => data.toArray
    }

    matlabEngine.putVariable("y", convert(yours))
    matlabEngine.putVariable("g", convert(golden))
    matlabEngine.eval(plotLine.head)
    matlabEngine.eval(s"hold on;")
    matlabEngine.eval(plotLine.last)
    matlabEngine.eval("legend('yours', 'golden')")
    matlabEngine.eval(s"title('$name')")

    matlabEngine.eval(s"saveas(gcf, 'simWorkspace/$name/$name', 'png')")
    logger.info(s"view the figure generated: /home/ltr/IdeaProjects/Chainsaw2/simWorkspace/$name/$name.png")
  }

  val constellation = Seq(
    s"plotFig = scatterplot(y, 1, 0, 'y.');",
    s"scatterplot(g, 1, 0, 'g.', plotFig)"
  )

  val array = Seq(
    s"plot(y, 'b')",
    s"plot(g, 'g')"
  )

  def scatterPlot[T: ClassTag](yours: Seq[T], golden: Seq[T], name: String) = draw( yours, golden, constellation, name)

  def plot[T: ClassTag](yours: Seq[T], golden: Seq[T], name: String) = draw(yours, golden, array, name)

}
