package org.datenlord
package xilinx

import xilinx.XilinxDeviceFamily._

import org.scalatest.flatspec.AnyFlatSpec

class VivadoReportTest extends AnyFlatSpec {

  //  val exampleLog = "/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/exampleLog.log"
  val exampleLog = "/home/ltr/IdeaProjects/Chainsaw2/synthWorkspace/graphFull1/doit.log"
  //  val exampleLog = "/home/ltr/IdeaProjects/Chainsaw2/synthWorkspace/UIntCompressor/doit.log"

  "Vivado Report" should "extract information from Vivado log file" in {
    val report = new VivadoReport(exampleLog, UltraScale)
    report.printArea()
    report.printFMax()
  }

}
