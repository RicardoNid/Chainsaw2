package org.datenlord
package zprize

import spinal.core._

import scala.collection.JavaConversions._
import scala.collection.mutable // as JGraphT is based on Java

object DagImplH {

  def apply(dag: Dag) = {

    implicit val ref: Dag = dag

    // pre-processing
    dag.autoPipeline()
    dag.doDrc()

    val signalMap = mutable.Map[DagPort, Bits]() // vertex with its output ports

    def getImplemented: Seq[DagVertex] = signalMap.keys.map(_.vertex).toSeq.distinct // vertices already implemented

    def getRemained: Seq[DagVertex] = dag.vertexSet().toSeq.diff(getImplemented) // vertices not implemented yet

    def getNextStage: Seq[DagVertex] = getRemained.filter(v => v.sources.forall(getImplemented.contains(_))) // vertices ready to be implemented

    def implVertex(target: DagVertex): Unit = {
      if (target.isIo) signalMap(target.out(0)) = signalMap(target.sourcePorts.head)
      else {
        val incomingEdges = target.incomingEdges.sortBy(_.inOrder)
        val drivingSignals = target.sourcePorts.map(signalMap)
        val pipelinedSignals = drivingSignals.zip(incomingEdges).map { case (signal, e) => signal.d(e.weight.toInt) }
        val core = target.gen.getImplH
        core.dataIn.zip(pipelinedSignals).foreach { case (port, bits) => port := bits }
        target.outPorts.zip(core.dataOut).foreach { case (port, bits) => signalMap(port) = bits }
      }
    }

    new ChainsawModule(dag) {
      dag.inputs.zip(dataIn).foreach { case (input, bits) => signalMap += input.out(0) -> bits }
      while (getNextStage.nonEmpty) {
        if (verbose >= 1) logger.info(s"implH next stage: ${getNextStage.mkString("\n")}")
        getNextStage.foreach(implVertex)
      }
      dag.outputs.zip(dataOut).foreach { case (output, port) => port := signalMap(output.out(0)) }
    }
  }

}
