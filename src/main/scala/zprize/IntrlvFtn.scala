//package org.datenlord
//package zprize
//
//import spinal.core._
//import spinal.core.sim._
//import spinal.lib._
//import spinal.lib.fsm._
//
//object IntrlvFtn extends ChainsawGenerator {
//  override def name = "intrlvFtn"
//
//  val coreGen = StridePermutation(intrlvRow, intrlvCol, parallel * 2, 1)
//
//  override val impl = coreGen.impl
//  override var inputTypes = coreGen.inputTypes
//  override var outputTypes = coreGen.outputTypes
//
//  override var inputFormat = codedFrameFormat
//  override var outputFormat = codedFrameFormat
//  override var latency = coreGen.latency
//
//  override def implH = ???
//
//  override def implNaiveH: ChainsawModule = new ChainsawModule(this) {
//    val core = coreGen.implNaiveH
//    core.validIn := validIn
//    core.lastIn := lastIn
//    core.dataIn := dataIn
//    dataOut := core.dataOut
//  }
//}
