package org.datenlord
package arithmetic

import org.scalatest.flatspec.AnyFlatSpec
import arithmetic.MultplierMode._

class KaratsubaTest extends AnyFlatSpec {

  "karatsuba graph" should "work for 377 bit full multiplication" in {
    Karatsuba(64, 0, FULL, true).toPng("kara64")
    Karatsuba(64, 0, FULL, true).validate().toPng("kara64validated")
  }

}
