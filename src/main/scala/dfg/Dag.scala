package org.datenlord
package dfg

import ilog.concert.IloNumVar
import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar
import org.jgrapht.alg.shortestpath.FloydWarshallShortestPaths
import org.jgrapht.graph._
import org.jgrapht._
import spinal.core._

import java.util.function.Supplier
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import ilog.cplex.IloCplex

import collection.JavaConversions._
import collection.JavaConverters._
import scala.math.{cos, log}

object OpType extends Enumeration {
  val Var = Value // variable (opposite to operation)
  val FullMult, LowMult, SquareMult, Add2, Add3, Sub2, Merge, Split, Resize = Value // Ring operations
  type OpType = Value
}

object Direction extends Enumeration {
  val In, Out = Value
  type Direction = Value
}

import dfg.Direction._
import dfg.OpType._

/** This is for vertices which do no operations, this can be used as input, output or intermediate variables
 *
 */
object VarVertex {
  def apply[TSoft, THard <: Data](name: String) =
    new DagVertex(name, 0, Var, (data: Seq[TSoft]) => data, (data: Seq[THard]) => data)
}

class DagPort[TSoft, THard <: Data](val vertex: DagVertex[TSoft, THard], val order: Int, val direction: Direction)

object DagPort {
  def apply[TSoft, THard <: Data](vertex: DagVertex[TSoft, THard], order: Int, direction: Direction): DagPort[TSoft, THard] = new DagPort(vertex, order, direction)
}


class Dag[TSoft, THard <: Data](val name: String)
  extends DirectedWeightedMultigraph[DagVertex[TSoft, THard], DagEdge](classOf[DagEdge]) {

  implicit val ref = this

  setEdgeSupplier(new Supplier[E] {
    override def get() = {
      val e = new E(0, 0)
      println(e.hashCode())
      e
    }
  })

  type V = DagVertex[TSoft, THard]
  type E = DagEdge
  type Port = DagPort[TSoft, THard]

  val inputs = ArrayBuffer[V]()
  val outputs = ArrayBuffer[V]()

  @deprecated
  override def addEdge(sourceVertex: DagVertex[TSoft, THard], targetVertex: DagVertex[TSoft, THard]) = super.addEdge(sourceVertex, targetVertex)

  @deprecated
  override def addEdge(sourceVertex: DagVertex[TSoft, THard], targetVertex: DagVertex[TSoft, THard], e: E) = super.addEdge(sourceVertex, targetVertex, e)

  def addEdge(source: Port, target: Port, weight: Double = 0) = {
    require(source.direction == Out && target.direction == In)
    val e = new E(target.order, source.order)
    assert(super.addEdge(source.vertex, target.vertex, e))
    setEdgeWeight(e, weight)
    e
  }

  def retiming(solution: Map[V, Int]): Unit = {
    edgeSet().foreach { e =>
      val newWeight = getEdgeWeight(e) + solution(getEdgeTarget(e)) - solution(getEdgeSource(e))
      setEdgeWeight(e, newWeight)
    }
  }

  //  old version implemented by optimus ojSolver, constraint by shortest path for shortest latency
  //  def validateByBound = {
  //    makeHomo()
  //    // declare model
  //    implicit val model: MPModel = MPModel(SolverLib.oJSolver)
  //    // declare variables, set bound for variables
  //    val vertices = vertexSet().toSeq
  //    val targetLatency = latencyLowerBound
  //    val variables: Seq[MPFloatVar] = vertices.map(v => MPFloatVar(v.toString, 0, targetLatency))
  //    val variableMap = Map(vertices.zip(variables): _*)
  //    // add feasibility constraints
  //    val constraints = edgeSet().toSeq.map { e =>
  //      val source = getEdgeSource(e)
  //      val target = getEdgeTarget(e)
  //      variableMap(source) - variableMap(target) <:= getEdgeWeight(e) - source.latency
  //    }
  //    subjectTo(constraints: _*)
  //    // set linear programming problem and solve it
  //    start()
  //    import scala.math.round
  //    val solution = variableMap.map { pair =>
  //      val value = pair._2.value.get
  //      if ((value - round(value)).abs > 0.1) logger.warn(s"bad solution $value")
  //      pair._1 -> round(value).toInt
  //    }
  //    retiming(solution)
  //    release()
  //    this
  //  }

  //  old version implemented by optimus ojSolver & cost function from VLSI DSP for minimum registers
  //  def validate() = {
  //
  //    makeHomo()
  //    // build graph with dummy vertices, and record weights of edges
  //    val originalEdges = edgeSet().toSeq
  //    val edgeWeights = mutable.Map[E, Double]()
  //    val ks = ArrayBuffer[Int]()
  //    vertexSet().toSeq
  //      .filter(v => outgoingEdgesOf(v).size() > 1)
  //      .foreach { v =>
  //        val outgoingEdges = outgoingEdgesOf(v).toSeq
  //        val k = outgoingEdges.size
  //        ks += k
  //        val latencies = outgoingEdges.map(getEdgeWeight)
  //        val latencyMax = latencies.max
  //        val targets = outgoingEdges.map(getEdgeTarget)
  //        val dummy = VarVertex[TSoft, THard](s"${v.name}_dummy")
  //        addVertex(dummy)
  //        val dummyEdges = targets.zip(latencies).map { case (target, latency) => addEdge(target.out(0), dummy.in(0), latencyMax - latency) }
  //        (outgoingEdges ++ dummyEdges)
  //          .foreach(e => edgeWeights += (e -> 1 / k.toDouble))
  //      }
  //    // declare model
  //    implicit val model: MPModel = MPModel(SolverLib.oJSolver)
  //    // declare variables
  //    val vertices = vertexSet().toSeq
  //    //    val targetLatency = latencyLowerBound
  //    val variables: Seq[MPFloatVar] = vertices.map(v => MPFloatVar(v.toString, 0, 50))
  //    val variableMap = Map(vertices.zip(variables): _*)
  //    // construct cost function for minimum number of registers
  //    val coeffs: Seq[Double] = vertices.map { v =>
  //      incomingEdgesOf(v).map(e => edgeWeights.getOrElse(e, 1.0)).sum -
  //        outgoingEdgesOf(v).map(e => edgeWeights.getOrElse(e, 1.0)).sum
  //    }
  //    val cost = variables.zip(coeffs)
  //      .map { case (variable, coeff) => variable * coeff }
  //      .reduce(_ + _)
  //    logger.info(s"optimus coeffs ${coeffs.mkString(" ")}")
  //    logger.info(s"optimus expr $cost")
  //    minimize(cost)
  //    // add feasibility constraints
  //    val constraints = originalEdges.map { e =>
  //      val source = getEdgeSource(e)
  //      val target = getEdgeTarget(e)
  //      val expr = variableMap(source) - variableMap(target) <:= getEdgeWeight(e) - source.latency
  //      logger.info(s"$source - $target < ${getEdgeWeight(e) - e.source.latency}")
  //      expr
  //    }
  //    subjectTo(constraints: _*)
  //    // set linear programming problem and solve it
  //    start()
  //    import scala.math.round
  //    val solution = variableMap.map { pair =>
  //      val value = pair._2.value.get
  //      if ((value - round(value)).abs > 0.1) logger.warn(s"bad solution $value")
  //      pair._1 -> round(value).toInt
  //    }
  //    retiming(solution)
  //    release()
  //    //     remove dummy vertices
  //    vertexSet().filter(_.name.endsWith("dummy")).foreach(removeVertex)
  //    this
  //  }


  /** Retiming for minimum latency under feasibility constraints
   */
  def validate() = {
    makeHomo()
    // TODO: find out why the cost built by fan out theorem(from VLSI DSP) always failed in cplex(and sometims failed in optimus)
    // declare model
    val cplex = new IloCplex()
    // declare variables
    val vertices = vertexSet().toSeq.toArray
    val lowerBounds = vertices.map(_ => 0.0)
    val upperBounds = vertices.map(_ => 200.0)
    val variables: Array[IloNumVar] = cplex.numVarArray(vertices.length, lowerBounds, upperBounds)
    val variableMap = vertices.zip(variables).toMap
    // construct cost function for minimum number of registers
    val coeffForSub = Array(1.0, -1.0)
    val io = Array(variableMap(outputs.head), variableMap(inputs.head))
    val cost = cplex.scalProd(io, coeffForSub) // shortest overall latency
    cplex.addMinimize(cost)
    // add feasibility constraints
    edgeSet().toSeq.foreach { e =>
      val source = variableMap(e.source)
      val target = variableMap(e.target)
      val expr = cplex.scalProd(Array(source, target), coeffForSub)
      cplex.addLe(expr, getEdgeWeight(e) - e.source.latency)
      //      logger.info(s"add constraint: ${e.source} - ${e.target} < ${getEdgeWeight(e) - e.source.latency}")
    }
    // set linear programming problem and solve it
    cplex.solve()
    import scala.math.round
    val solution = vertices.zip(cplex.getValues(variables).map(round).map(_.toInt)).toMap
    logger.info(s"solution status: ${cplex.getStatus}")
    logger.info(s"solution latency = ${round(cplex.getObjValue())}")
    cplex.end()
    // retiming by the solution
    retiming(solution)
    this
  }

  // TODO: merge implH and implS
  def implH: Seq[THard] => Seq[THard] = (dataIns: Seq[THard]) => {

    doDrc()

    val signalMap = mutable.Map[V, Seq[THard]]()

    // vertices already implemented
    def implemented: Seq[V] = signalMap.keys.toSeq

    // vertices not implemented yet
    def remained: Seq[V] = vertexSet().toSeq.diff(implemented)

    // vertices ready to be implemented
    def nextStage: Seq[V] = remained.filter(v => v.sources.forall(implemented.contains(_)))

    def implVertex(target: V): Unit = {
      val incomingEdges = incomingEdgesOf(target).toSeq.sortBy(_.inOrder)
      val dataIns = incomingEdges.map { e =>
        val signal = signalMap(getEdgeSource(e))(e.outOrder)
        signal.d(getEdgeWeight(e).toInt - getEdgeSource(e).latency)
      }
      val rets = target.implH(dataIns)
      signalMap += target -> rets
    }

    // initialize signalMap
    inputs.zip(dataIns).foreach { case (input, bits) => signalMap += input -> Seq(bits) }
    // implement vertices until no available vertices exist
    while (nextStage.nonEmpty) nextStage.foreach(implVertex)

    if (remained.nonEmpty) logger.warn(s"isolated nodes exist:\n${remained.mkString(" ")}")

    outputs.flatMap(signalMap(_))
  }

  def implS = (dataIns: Seq[TSoft]) => {

    val signalMap = mutable.Map[V, Seq[TSoft]]()

    // vertices already implemented
    def implemented: Seq[V] = signalMap.keys.toSeq

    // vertices not implemented yet
    def remained: Seq[V] = vertexSet().toSeq.diff(implemented)

    // vertices ready to be implemented
    def nextStage: Seq[V] = remained.filter(v => v.sources.forall(implemented.contains(_)))

    def implVertex(target: V): Unit = {
      val incomingEdges = incomingEdgesOf(target).toSeq.sortBy(_.inOrder)
      val dataIns = incomingEdges.map(e =>
        signalMap(getEdgeSource(e))(e.outOrder))
      val rets = target.implS(dataIns)
      signalMap += target -> rets
    }

    // initialize signalMap
    inputs.zip(dataIns).foreach { case (input, bits) => signalMap += input -> Seq(bits) }
    // implement vertices until no available vertices exist
    while (nextStage.nonEmpty) nextStage.foreach(implVertex)

    if (remained.nonEmpty) logger.warn(s"isolated nodes exist:\n${remained.mkString(" ")}")

    outputs.flatMap(signalMap(_))
  }

  def evaluateS(dataIns: Seq[TSoft]) = implS.apply(dataIns)

  def evaluateH(dataIns: Seq[THard]) = implH.apply(dataIns)

  def simplified() = {
    val inters = vertexSet().toSeq // find inters
      .filter(_.opType == Var)
      .filterNot(_.isIo)
      .filter(v => v.inDegree == 1 && v.outDegree == 1)
    inters.foreach { inter => // remove one by one
      val source = inter.sourcePorts.head
      val target = inter.targetPorts.head
      val weight = inter.incomingEdges.head.weight + inter.outgoingEdges.head.weight
      addEdge(source, target, weight)
      assert(removeVertex(inter))
    }
    logger.info("redundant vertices eliminated")
    makeHomo()

    this
  }

  def addGraphsAfter(source: Dag[TSoft, THard], starts: Seq[Port]): Seq[DagPort[TSoft, THard]] = {
    require(source.inputs.length == starts.length)
    require(starts.forall(_.direction == Out))
    //    require(starts.forall(_.vertex.outDegree == 0))
    // add
    Graphs.addGraph(this, source) // add all vertices and edges of that to this, but the edge weights won't be copied
    source.edgeSet().foreach(e => setEdgeWeight(e, source.getEdgeWeight(e))) // copy the edge weights
    // link
    starts.zip(source.inputs).foreach { case (port, in) => addEdge(port, in.in(0)) }
    // return output ports of source graph, which are part of this graph now
    source.outputs.map(_.out(0))
  }

  def addGraphBetween(source: Dag[TSoft, THard], starts: Seq[Port], ends: Seq[Port]): Unit = {
    require(source.outputs.length == ends.length)
    require(ends.forall(_.direction == In))
    require(ends.forall(_.vertex.inDegree == 0))
    // add & link inputs
    val outputs = addGraphsAfter(source, starts)
    // link outputs
    ends.zip(outputs).foreach { case (port, out) => addEdge(out, port) }
  }

  def isHomo(): Boolean = edgeSet().forall(_.weight == 0)

  def makeHomo(): Unit = {
    if (!isHomo()) {
      edgeSet().foreach(e => setEdgeWeight(e, 0.0))
    }
  }

  def checkAcyclic() = {
    val detector = new alg.cycle.CycleDetector(this)
    assert(!detector.detectCycles())
  }

  def getIoPath: Seq[V] = {
    var current = inputs.head
    val path = ArrayBuffer[V](current)
    while (!outputs.contains(current)) {
      current = current.targets.head
      path += current
    }
    path
  }

  /** This latency is true only when the graph is homogeneous
   */
  def latency = getIoPath.init.zip(getIoPath.tail)
    .map { case (s, t) => getEdge(s, t).weight }.sum.toInt

  def pathToString(path: GraphPath[V, E]) = {
    path.getVertexList.zip(path.getEdgeList)
      .map { case (v, e) => s"$v -> ${e.weight} -> " }.mkString("\n") +
      path.getEndVertex.toString
  }

  def latencyLowerBound = {
    makeHomo()
    edgeSet().toSeq.foreach(e => setEdgeWeight(e, -e.source.latency))
    val algo = new FloydWarshallShortestPaths(this)
    val paths = inputs.flatMap(in => outputs.map(out => algo.getPath(in, out)))
    val shortestPath = paths.filterNot(_ == null).minBy(_.getWeight)
    logger.info(s"longest path:\n${pathToString(shortestPath)}")
    val min = shortestPath.getWeight
    makeHomo()
    logger.info(s"minimum latency of current graph is ${-min}")
    -min
  }

  /** Add design rules here, invoked before impl
   *
   */
  def doDrc() = {
    checkAcyclic()
    assert(vertexSet().filter(_.latency > 0).forall(_.outDegree > 0))
  }

  def showCost = {
    val costOfOps = OpType.values.map(op => s"$op -> ${vertexSet().count(_.opType == op)}").mkString("\n\t")
    logger.info(s"number of operators:\n\t$costOfOps")
  }

  override def toString =
    s"vertices:\n${vertexSet().mkString("\n")}\n" +
      s"edges:\n${edgeSet().map(_.toStringInGraph).mkString("\n")}"
}
