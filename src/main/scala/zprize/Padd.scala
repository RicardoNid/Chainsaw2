package org.datenlord
package zprize

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.traverse._
import org.jgrapht.generate._
import scala.collection.JavaConversions._ // as JGraphT is based on Java

import cc.redberry.rings

import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._
import rings.primes._

import algos._

case class Padd() extends Dag {
  override val name = "padd"

  implicit val ec: EcGroup = algos.ZPrizeMSM.ec
  override val impl = (dataIn: Seq[Any]) => {
    val Seq(x0, y0, z0, x1, y1, z1) = dataIn.asInstanceOf[Seq[BigInt]]
    val p0 = EcPointProj(x0, y0, z0)
    val p1 = EcPointProj(x1, y1, z1)
    val ret = ec.paddHomo(p0, p1)
    Seq(ret.x, ret.y, ret.z).map(_.toBigInt)
  }

  override val frameFormat = frameNoControl

  override val inputType = HardType(UInt())
  override val outputType = HardType(UInt())


}
