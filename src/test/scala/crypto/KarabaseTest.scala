package crypto

import org.datenlord.zprize.Karabase
import org.datenlord.{ChainsawImpl, ChainsawTest}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class KarabaseTest extends AnyFlatSpec {

  behavior of "karabase"

  val widthA = 16
  val widthB = 25
  val gen = Karabase(widthA, widthB)
  def batch: Seq[BigInt] = Seq(widthA, widthA, widthB, widthB).map(BigInt(_, Random))
  val data = Seq.fill(100)(batch).flatten

  it should "work" in ChainsawTest.test(gen, data)

  it should "impl" in ChainsawImpl(gen, withRequirement = true)

}
