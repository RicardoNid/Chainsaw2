package org.datenlord
package arithmetic

import org.scalatest.flatspec.AnyFlatSpec
import arithmetic.MultplierMode._

class KaratsubaTest extends AnyFlatSpec {

  "karatsuba graph" should "work for 377 bit full multiplication" in {
    Karatsuba(377, 0, FULL, true).validate().showCost // show cost
    Karatsuba(377, 0, HALFLOW, true).validate().showCost
  }

}
