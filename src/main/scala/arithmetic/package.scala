package org.datenlord

package object arithmetic {

  object MultiplierMode extends Enumeration {
    val FULL, LSB, MSB, SQUARE, FULL34, COMPLEX = Value
    type MultiplierMode = Value
  }

  object AdderMode extends Enumeration {
    val BINARY, TERNARY = Value
    type AdderMode = Value
  }



}

