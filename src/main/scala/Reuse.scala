package org.datenlord

case class Reuse(spaceReuse: Int, timeReuse: Int, spaceFold: Int, timeFold: Int) {

  require(spaceFold == 1 || timeFold == 1)

}

object Reuse {
  def unit = Reuse(1,1,1,1)
}
