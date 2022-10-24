package examples

import examples.Companion.sum

class Companion(private val a: Int, private val b: Int) {

  sum(this)

}

object Companion {

  def sum(companion: Companion) = {
    companion.a + companion.b
  }

}
