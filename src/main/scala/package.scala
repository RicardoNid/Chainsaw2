package org

import breeze.linalg._
import breeze.math._
import cc.redberry.rings
import cc.redberry.rings.scaladsl._
import org.datenlord.xilinx._
import org.slf4j.LoggerFactory
import spinal.core._
import spinal.core.sim._
import spinal.lib.{Delay, _}

import scala.collection.mutable.ArrayBuffer
import scala.math.{BigDecimal, BigInt}
import scala.reflect.ClassTag
import scala.sys.process.Process
import scala.util.Random


package object datenlord {

  val logger = LoggerFactory.getLogger("Chainsaw logger")

  type ChainsawFlow[T <: Data] = Flow[Fragment[Vec[T]]]

  implicit class ChainsawFlowUtil[T <: Data](flow: ChainsawFlow[T]) {

    def withFragment(fragment: Vec[T]): ChainsawFlow[T] = ChainsawFlow(fragment, flow.valid, flow.last)

    def withFragment(fragment: Seq[T]): ChainsawFlow[T] = withFragment(Vec(fragment))

    def d(cycle: Int) = ChainsawFlow(flow.fragment.d(cycle), flow.valid.d(cycle), flow.last.d(cycle))

  }

  def VivadoImpl[T <: Component](gen: => T, name: String = "temp", xdcPath: String = null) = {
    val report = VivadoFlow(design = gen, taskType = IMPL, topModuleName = name, workspacePath = s"./$name").doFlow()
    report.printArea()
    report.printFMax()
    report
  }

  def VivadoSynth[T <: Component](gen: => T, name: String = "temp"): VivadoReport = {
    val report = VivadoFlow(design = gen, taskType = SYNTH, topModuleName = name, workspacePath = s"./$name").doFlow()
    report.printArea()
    report.printFMax()
    report
  }

  implicit class DataUtil[T <: Data](data: T) {
    def d(cycle: Int): T = Delay(data, cycle)

    def validAfter(cycle: Int) = Delay(data, cycle, init = False).asInstanceOf[Bool]

    def split(splitPoints: Seq[Int]): Seq[Bits] = {
      var current = data.asBits
      val ret = ArrayBuffer[Bits]()
      splitPoints.foreach{ point =>
        val (high, low) = current.splitAt(point)
        ret += high
        current = low
      }
      ret += current
      ret
    }
  }

  implicit class BigIntUtil(bi: BigInt) {

    /**
     * @example 10100.splitAt(3) = (10,100), not (101,11)
     */
    def split(lowWidth: Int) = (bi >> lowWidth, bi % (BigInt(1) << lowWidth))

    def split(splitPoints: Seq[Int]): Seq[BigInt] = {
      var current = bi
      val ret = ArrayBuffer[BigInt]()
      splitPoints.foreach{ point =>
        val (high, low) = current.split(point)
        ret += high
        current = low
      }
      ret += current
      ret
    }

    /**
     * @example 10100.slice(3 downto 1) = 010
     */
    def slice(range: Range.Inclusive) = {
      val from = bi.bitLength - range.head - 1
      val until = bi.bitLength - range.last
      BigInt(bi.toString(2).slice(from, until), 2)
    }

    /** Get words from low to high, the highest word can be incomplete
     */
    def toWords(wordWidth: Int) = bi.toString(2)
      .reverse.grouped(wordWidth).toSeq
      .map(digits => BigInt(digits.reverse, 2))
  }

  implicit class DMUtil[T: ClassTag](matrix: DenseMatrix[T]) {
    def diag = {
      require(matrix.rows == matrix.cols)
      val n = matrix.rows
      new DenseVector((0 until n).map(i => matrix(i, i)).toArray)
    }
  }

  implicit class StringUtil(s: String) {
    def padToLeft(len: Int, elem: Char) = s.reverse.padTo(len, elem).reverse

    def repeat(times: Int) = Seq.fill(times)(s).reduce(_ + _)
  }

  implicit class seqUtil[T: ClassTag](seq: Seq[T]) {
    def divide(group: Int) = seq.grouped(seq.length / group).toSeq

    def prevAndNext[TOut](f: ((T, T)) => TOut) = seq.init.zip(seq.tail).map(f)

    def padToLeft(len: Int, elem: T) = seq.reverse.padTo(len, elem).reverse
  }

  def logR(n: Int, radix: Int) = {
    var current = n
    var ret = 0
    while (current > 1) {
      require(current % radix == 0)
      current /= radix
      ret += 1
    }
    ret
  }

  def powR(radix: Int, exp: Int) = Seq.fill(exp)(radix).product

  implicit class RandomUtil(rand: Random) {

    def RandomSequence[T: ClassTag](length: Int, randGen: () => T) = (0 until length).map(_ => randGen())

    def RandomSequences[T: ClassTag](count: Int, length: Int, randGen: () => T) = (0 until count).map(_ => RandomSequence(length, randGen))

    def RandomVectors[T: ClassTag](count: Int, length: Int, randGen: () => T) =
      RandomSequences(count, length, randGen).map(seq => new DenseVector(seq.toArray))

    def nextComplex(): Complex = new Complex(rand.nextDouble(), rand.nextDouble())

    def RandomComplexVectors(count: Int, length: Int) = RandomVectors(count, length, nextComplex)

    def RandomComplexSequences(count: Int, length: Int) = RandomSequences(count, length, nextComplex)

    def nextBits(bitLength: Int): Seq[Int] = rand.nextString(bitLength).map(_ % 2)

    def nextBinaryString(bitLength: Int): String = nextBits(bitLength).mkString("")

    def nextBigInt(bitLength: Int) = BigInt(rand.nextBinaryString(bitLength), 2)

    def RandomBigInts(count: Int, length: Int) = (0 until count).map(_ => (0 until length).map(_ => nextBigInt(length)))
  }

  implicit class SFixUtil(sf: SFix) {
    def unary_-() = {
      val ret = SFix(sf.maxExp exp, sf.minExp exp)
      ret.raw := -sf.raw
      ret
    }

    def doAddSub(that: SFix, add: Boolean) = {
      val (rawLeft, rawRight) = sf.alignLsb(that)
      val ret = SFix(Math.max(sf.maxExp, that.maxExp) + 1 exp, Math.max(rawLeft.getBitsWidth, rawRight.getBitsWidth) + 1 bits)
      ret.raw := (if (add) rawLeft +^ rawRight else rawLeft -^ rawRight)
      ret
    }

    def +^(that: SFix) = doAddSub(that, true)

    def -^(that: SFix) = doAddSub(that, false)

    def abs = {
      val ret = cloneOf(sf)
      ret.raw := sf.raw.abs.asSInt
      ret
    }

    def isPositive = ~sf.raw.msb

    def isNegative = sf.raw.msb

    def truncated(dataType: HardType[SFix]) = {
      val ret = dataType()
      ret := sf.truncated
      ret
    }

    import sf._

    // for sfix simulation
    def #=(value: BigDecimal): Unit = { // copied from SF object
      assert(value <= maxValue, s"Literal $value is too big to be assigned in $this")
      assert(value >= minValue, s"Literal $value is too small to be assigned in this $this")

      val shift = -minExp
      val ret = if (shift >= 0) // ret this is the "binary string" of value at specific precision
        (value * BigDecimal(BigInt(1) << shift)).toBigInt
      else
        (value / BigDecimal(BigInt(1) << -shift)).toBigInt
      setLong(raw, ret.toLong)
    }

    def #=(value: Double): Unit = #=(BigDecimal(value))

    def toDouble = raw.toBigInt.toDouble / (1 << -minExp)

  }

  implicit class SimComplexPimper(cn: ComplexFix) {
    def #=(value: Complex): Unit = {
      cn.real #= value.real
      cn.imag #= value.imag
    }

    def toComplex = new Complex(cn.real.toDouble, cn.imag.toDouble)
  }

  implicit class VecUtil[T <: Data](vec: Vec[T]) {
    def :=(that: Seq[T]) = vec.zip(that).foreach { case (port, data) => port := data }
  }

  def factors(value: Int) = (1 to value).filter(value % _ == 0)

  implicit def base2transform(base: TransformBase) = base.toTransformMesh

  implicit def mesh2system(mesh: TransformMesh) = mesh.toSystem

  implicit class normalOps(intZ: IntZ) {
    def toBigInt = BigInt(intZ.toByteArray)
  }

  def doCmd(command: String, path: String): Unit = { // do cmd at the workSpace
    println(command)
    val isWindows = System.getProperty("os.name").toLowerCase().contains("win")
    if (isWindows)
      Process("cmd /C " + command, new java.io.File(path)) !
    else
      Process(command, new java.io.File(path)) !
  }
}
