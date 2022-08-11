package org.datenlord

package object arithmetic {

  object MultplierMode extends Enumeration {
    val FULL, HALFLOW, HALFHIGH, SQUARE, FULL34, COMPLEX = Value
    type MultiplierMode = Value
  }

}
