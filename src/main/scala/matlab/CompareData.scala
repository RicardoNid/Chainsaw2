package org.datenlord
package matlab

import scala.io.StdIn
import scala.reflect.ClassTag

/** draw a figure by Matlab to compare two sequences of data
 *
 */
object CompareData {

  def apply[T: ClassTag](yours: Seq[T], golden: Seq[T], name: String) = {
    matlabEngine.putVariable("y", yours.toArray)
    matlabEngine.putVariable("g", golden.toArray)
    matlabEngine.eval(s"plot(y, 'b')")
    matlabEngine.eval("hold on;")
    matlabEngine.eval(s"plot(g, 'g')")
    matlabEngine.eval("legend('yours', 'golden')")
    matlabEngine.eval(s"title('$name')")
  }
}

object SaveCurrentFigure {
  def apply(name: String) = matlabEngine.eval(s"saveas(gcf, '/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/matlabGenerated/$name', 'png')")
}