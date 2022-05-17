package org.datenlord

case class Reuse(spaceReuse: Int, timeReuse: Int, spaceFold: Int, timeFold: Int) {

  require(spaceFold == 1 || timeFold == 1)

}
