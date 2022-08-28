package org.datenlord

object ImplMode

sealed trait ImplMode
object Comb extends ImplMode
object StateMachine  extends ImplMode
object Infinite  extends ImplMode