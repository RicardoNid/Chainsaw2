package org.datenlord
package xilinx

import org.apache.commons.io.FileUtils
import spinal.core._

import java.io.File
import scala.collection.mutable
import scala.language.postfixOps
import scala.sys.process._
import xilinx.VivadoTaskType._

import java.nio.file.Paths
import scala.io.Source

/** used to generate sources for a Vivado flow and invoke Vivado to run it
 */
class VivadoFlow[T <: Component](
                                  design: => T,
                                  taskType: VivadoTaskType,
                                  vivadoConfig: VivadoConfig,
                                  xilinxDevice: XilinxDevice,
                                  topModuleName: String = null,
                                  workspacePath: String = null,
                                  alterXdc: String = null,
                                  extraRtlSources: Seq[String] = null
                                ) {

  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")

  /** core function of Vivado flow, used to run a synth/impl task
   *
   * @return VivadoReport as an object
   */
  def doFlow(): VivadoReport = {
    usePrimitives = true
    useFlopoco = true
    // create workspace directory
    Process(s"mkdir -p $workspacePath") !
    // generate sources from dut
    val spinalReport = SpinalConfig(targetDirectory = workspacePath, oneFilePerComponent = true)
      .generateVerilog(design.setDefinitionName(topModuleName))
    // generate xdc & tcl file
    FileUtils.write(
      new File(s"$workspacePath/doit.tcl"),
      buildScript(spinalReport.rtlSourcesPaths)
    )
    FileUtils.write(new File(s"$workspacePath/doit.xdc"), buildXdc)
    // run vivado
    doCmd(
      s"${vivadoConfig.vivadoPath}/vivado -stack 2000 -nojournal -log doit.log -mode batch -source doit.tcl",
      workspacePath
    )
    // parse log file to get report
    new VivadoReport(s"$workspacePath/doit.log", xilinxDevice.family)
  }

  /** generate XDC constraints content for Vivado Flow
   *
   * priority: alter xdc > device xdc > xdc built from fmax
   *
   * @return XDC constraints
   */
  def buildXdc: String = {
    if (alterXdc != null) alterXdc
    else if (xilinxDevice.constraint != null) xilinxDevice.constraint
    else {
      val targetPeriod = xilinxDevice.fMax.toTime
      s"""create_clock -period ${(targetPeriod * 1e9) toBigDecimal} [get_ports clk]"""
    }
  }

  /** generate tcl script content for Vivado Flow
   *
   * @param dutRtlSources paths of rtl sources generated by dut
   * @return context of tcl script
   */
  def buildScript(dutRtlSources: mutable.LinkedHashSet[String]): String = {
    var script = ""

    def getReadCommand(sourcePath: String) = {
      if (sourcePath.endsWith(".sv")) s"read_verilog -sv $sourcePath \n"
      else if (sourcePath.endsWith(".v")) s"read_verilog $sourcePath \n"
      else if (sourcePath.endsWith(".vhdl") || sourcePath.endsWith(".vhd"))
        s"read_vhdl $sourcePath \n"
      else if (sourcePath.endsWith(".bin")) "\n"
      else
        throw new IllegalArgumentException(
          s"invalid RTL source path $sourcePath"
        )
    }

    // read design sources
    // sources from dut
    val lstFile = Source.fromFile(workspacePath + "/" + s"$topModuleName.lst")
    lstFile.getLines().foreach { line =>
      if (line.startsWith("/")) script += s"read_verilog $line\n" // absolute path
      else script += s"read_verilog ${line.split("/").last}\n" // relative path
    }
    lstFile.close()

    //    dutRtlSources
    //      .map(_.replace(workspacePath + "/", ""))
    //      .foreach(script += getReadCommand(_))

    // extra sources
    if (extraRtlSources != null)
      extraRtlSources.foreach(script += getReadCommand(_))
    // read constraint sources
    script += s"read_xdc doit.xdc\n"
    // do flow
    taskType match {
      case SYNTH =>
        script += s"synth_design -part ${xilinxDevice.part} -top ${topModuleName} -mode out_of_context\n"
        script += s"write_checkpoint -force ${topModuleName}_after_synth.dcp\n"
      case IMPL =>
        script += s"synth_design -part ${xilinxDevice.part} -top ${topModuleName} -mode out_of_context\n"
        script += s"write_checkpoint -force ${topModuleName}_after_synth.dcp\n"
        script += "opt_design\n"
        script += "place_design\n"
        script += s"write_checkpoint -force ${topModuleName}_after_place.dcp\n"
        script += "route_design\n"
        script += s"write_checkpoint -force ${topModuleName}_after_route.dcp\n"
    }
    // util & timing can't be reported after synth/impl
    script += s"report_utilization\n"
    script += s"report_timing\n"
    script
  }
}

object VivadoFlow {

  /** default configurations for Vivado flow are set here
   */
  def apply[T <: Component](
                             design: => T,
                             taskType: VivadoTaskType,
                             vivadoConfig: VivadoConfig = VivadoConfig(),
                             xilinxDevice: XilinxDevice = defaultDevice,
                             topModuleName: String = null,
                             workspacePath: String = null,
                             alterXdc: String = null,
                             extraRtlSources: Seq[String] = null
                           ): VivadoFlow[T] =
    new VivadoFlow(
      design,
      taskType,
      vivadoConfig,
      xilinxDevice,
      topModuleName,
      workspacePath,
      alterXdc,
      extraRtlSources
    )
}
