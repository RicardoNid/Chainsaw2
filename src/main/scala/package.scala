package org

import breeze.linalg._
import breeze.math._
import cc.redberry.rings.scaladsl._
import com.mathworks.engine._
import org.datenlord.xilinx.VivadoTaskType._
import org.datenlord.xilinx._

import scala.collection.mutable
import org.slf4j.LoggerFactory
import spinal.core._
import spinal.core.sim._
import spinal.lib.{Delay, com => _, _}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.math.{BigDecimal, BigInt}
import scala.reflect.ClassTag
import scala.sys.process.Process
import scala.util.Random

package object datenlord {

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    val (p, q) = if (a >= b) (a, b) else (b, a)
    if (q == 0) p
    else gcd(q, p % q)
  }

  def mux[T](condition: Boolean, whenTrue: T, whenFalse: T) = if(condition) whenTrue else whenFalse

  type Metric = (Seq[Any], Seq[Any]) => Boolean

  // adjustable parameters

  val logger = LoggerFactory.getLogger("Chainsaw logger")
  var verbose = 0

  var skipComponentSim = false

  // record all distinct Chainsaw Modules
  var generatorList = mutable.Map[String, Int]()
  val naiveSet = mutable.Set[String]()
  var implTime = false

  type ChainsawFlow[T <: Data] = Flow[Fragment[Vec[T]]]

  implicit class ChainsawFlowUtil[T <: Data](flow: ChainsawFlow[T]) {

    def withFragment(fragment: Seq[T]): ChainsawFlow[T] = ChainsawFlow(Vec(fragment), flow.valid, flow.last)

    def replaceBy(func: Seq[T] => Seq[T]): ChainsawFlow[T] = withFragment(func(flow.fragment))

    def withLast(last: Bool): ChainsawFlow[T] = ChainsawFlow(flow.fragment, flow.valid, last)

    // TODO: is this proper?
    def d(cycle: Int): Flow[Fragment[Vec[T]]] = ChainsawFlow(flow.fragment.d(cycle), flow.valid.d(cycle), flow.last.d(cycle))

  }

  def RtlGen[T <: Component](gen: => T, name: String = "temp") = {
    val genDir = s"/home/ltr/IdeaProjects/Chainsaw2/genWorkspace/$name/"
    val ret = SpinalConfig(targetDirectory = genDir, oneFilePerComponent = true)
      .generateVerilog(gen.setDefinitionName(name))
      .printPrunedIo()
    logger.info(s"\nrtl generated: \n${ret.rtlSourcesPaths.mkString("\n")}")
  }

  import zprize._

  def VivadoImpl[T <: Component](gen: => T, name: String = "temp", target: XilinxDevice = defaultDevice, xdcPath: String = null) = {
    val report = VivadoFlow(design = gen, taskType = IMPL, topModuleName = name, workspacePath = s"synthWorkspace/$name/", xilinxDevice = target, alterXdc = xdcPath).doFlow()
    report.printArea()
    report.printFMax()
    report
  }

  def VivadoSynth[T <: Component](gen: => T, name: String = "temp", target: XilinxDevice = defaultDevice): VivadoReport = {
    val report = VivadoFlow(design = gen, taskType = SYNTH, topModuleName = name, workspacePath = s"synthWorkspace/$name/", xilinxDevice = target).doFlow()
    report.printArea()
    report.printFMax()
    report
  }

  def ChainsawImpl(gen: => ChainsawGenerator, name: String = "temp", target: XilinxDevice = defaultDevice, xdcPath: String = null, withRequirement: Boolean = false) = {
    implTime = true
    val report = VivadoImpl(gen.implH, name, target, xdcPath)
    implTime = false
    if (withRequirement) report.require(gen.utilEstimation, gen.fmaxEstimation)
    report
  }

  def ChainsawSynth(gen: => ChainsawGenerator, name: String = "temp", target: XilinxDevice = defaultDevice, withRequirement: Boolean = false) = {
    implTime = true
    val report = VivadoSynth(gen.implH, name, target)
    implTime = false
    if (withRequirement) report.require(gen.utilEstimation, gen.fmaxEstimation)
    report
  }

  implicit class BoolUtil(data: Bool) {
    def validAfter(cycle: Int) = Delay(data, cycle, init = False)

    def setAfter(cond: Bool, cycle: Int) = when(cond.d(cycle))(data.set())
  }

  implicit class DataUtil[T <: Data](data: T) {
    def d(cycle: Int = 1): T = Delay(data, cycle)

    def split(splitPoints: Seq[Int]): Seq[Bits] = {
      var current = data.asBits
      val ret = ArrayBuffer[Bits]()
      splitPoints.foreach { point =>
        val (high, low) = current.splitAt(point)
        ret += high
        current = low
      }
      ret += current
      ret
    }
  }

  implicit class BigIntUtil(bi: BigInt) {

    def to2sComplement =
      if (bi > 0) s"0${bi.toString(2)}"
      // TODO: better implementation for 0 and -1
      else if (bi == 0) "00"
      else if (bi == -1) "11"
      else {
        val complement = (BigInt(1) << bi.bitLength) + bi
        s"1${complement.toString(2).padToLeft(bi.bitLength, '0')}"
      }

    def from2sComplement(bits: String) = {
      val sign = bits.take(1)
      val main = BigInt(bits.tail, 2)
      main - (BigInt(sign, 2) << (bits.length - 1))
    }

    /**
     * @example 10100.splitAt(3) = (10,100), not (101,11)
     */
    def split(lowWidth: Int) = {
      val base = BigInt(1) << lowWidth
      if (bi < BigInt(0)) (bi >> lowWidth, if (bi % base < 0) bi % base + base else bi % base)
      else (bi >> lowWidth, bi % base)
    }

    /** Split BigInt into segments according to split point
     *
     * @param splitPoints split points high to low
     * @return segments low to high
     */
    def split(splitPoints: Seq[Int]): Seq[BigInt] = {
      require(splitPoints.sorted.reverse.equals(splitPoints), "split points should be descending order")
      var current = bi
      val ret = ArrayBuffer[BigInt]()
      splitPoints.foreach { point =>
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

    def takeLow(n: Int) = {
      require(bi >= BigInt(0), s"$bi")
      bi.split(n)._2
    }

    def takeHigh(n: Int) = {
      require(bi >= BigInt(0), s"$bi")
      bi.split(bi.bitLength - n)._1
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

    def truncate(dataType: HardType[SFix]) = {
      val ret = dataType()
      ret := sf.truncated
      ret
    }

    // for sfix simulation
    //    def #=(value: BigDecimal): Unit = { // copied from SF object
    //      assert(value <= maxValue, s"Literal $value is too big to be assigned in $this")
    //      assert(value >= minValue, s"Literal $value is too small to be assigned in this $this")
    //
    //      val shift = -minExp
    //      val ret = if (shift >= 0) // ret this is the "binary string" of value at specific precision
    //        (value * BigDecimal(BigInt(1) << shift)).toBigInt
    //      else
    //        (value / BigDecimal(BigInt(1) << -shift)).toBigInt
    //      setLong(raw, ret.toLong)
    //    }

    //    def #=(value: Double): Unit = #=(BigDecimal(value))

    //    def toDouble = raw.toBigInt.toDouble / (1 << -minExp)

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

    def vecShiftWrapper(bitsShift: UInt => Bits, that: UInt): Vec[T] = {
      val ret = cloneOf(vec)
      val shiftedBits: Bits = bitsShift((that * widthOf(vec.dataType)).resize(log2Up(widthOf(vec.asBits))))
      ret.assignFromBits(shiftedBits)
      ret
    }

    val bits = vec.asBits

    def rotateLeft(that: Int): Vec[T] = vecShiftWrapper(bits.rotateRight, that)

    def rotateLeft(that: UInt): Vec[T] = vecShiftWrapper(bits.rotateRight, that)

    def rotateRight(that: Int): Vec[T] = vecShiftWrapper(bits.rotateLeft, that)

    def rotateRight(that: UInt): Vec[T] = vecShiftWrapper(bits.rotateLeft, that)
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

  def doCmdAndGetLines(command: String, path: String): String = { // do cmd at the workSpace
    println(command)
    val isWindows = System.getProperty("os.name").toLowerCase().contains("win")
    if (isWindows)
      Process("cmd /C " + command, new java.io.File(path)) !!
    else
      Process(command, new java.io.File(path)) !!
  }

  var usePrimitives = false
  var useFlopoco = false
  val unisimPath = "/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/unisims"

  import org.scalatest.Tag

  object SynthTest extends Tag("datenlord.tags.Synth")

  var binaryAddLimit = 96
  var ternaryAddLimit = 96

  lazy val matlabEngine = MatlabEngine.startMatlab()

  implicit class ArrayUtil[T](array: Array[T]) {
    def toMatlabLiteral = s"[${array.mkString(",")}]"
  }

  object SFConstant {
    def apply(value: BigDecimal, peak: ExpNumber, width: BitCount): SFix = {
      val tmp = SFix(peak, width)
      tmp := value
      tmp
    }

    def apply(value: BigDecimal, peak: ExpNumber, resolution: ExpNumber): SFix = {
      val tmp = SFix(peak, resolution)
      tmp := value
      tmp
    }

    def apply(value: BigDecimal, hardType: HardType[SFix]): SFix = {
      val tmp = hardType()
      tmp := value
      tmp
    }
  }

  implicit class IntUtil(int: Int) {
    def divideAndCeil(base: Int) = (int + base - 1) / base

    def nextMultiple(base: Int) = divideAndCeil(base) * base
  }


  sealed trait OperatorType

  sealed trait AdderType extends OperatorType

  object BinaryAdder extends AdderType

  object BinarySubtractor extends AdderType

  object TernaryAdder extends AdderType

  object TernarySubtractor1 extends AdderType

  object TernarySubtractor2 extends AdderType

  object Compressor extends AdderType

  trait MultiplierType extends OperatorType

  object FullMultiplier extends MultiplierType

  object SquareMultiplier extends MultiplierType

  object MsbMultiplier extends MultiplierType

  object LsbMultiplier extends MultiplierType

  object Kara extends MultiplierType

  trait VarType extends OperatorType

  object Input extends VarType

  object Output extends VarType

  object Var extends VarType

  object Custom extends OperatorType

  object Multiplexer extends OperatorType

  object And extends OperatorType

  object Shift extends OperatorType

  object Split extends OperatorType

  object Merge extends OperatorType

  object Resize extends OperatorType

  sealed trait SopStructure

  object Direct extends SopStructure

  object Transpose extends SopStructure

  object Systolic extends SopStructure

  sealed trait Direction

  object In extends Direction

  object Out extends Direction

}
