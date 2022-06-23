package org.datenlord
package ip.das

import spinal.core._

import java.io._
import java.nio.file.Paths
import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._

object CreateProject {

  def apply[T <: Component](workspace: String, projectName: String, top: => T) = {

    val projectSpace = s"$workspace/$projectName"

    s"mkdir $projectSpace \n".run()

    val scriptCreate = new File("/home/ltr/IdeaProjects/Chainsaw2/src/main/scala/ip/das/CreateProject.tcl")
    val linesCreate = Source.fromFile(scriptCreate).getLines()
      .map(_.replace("LvdsWithPcie", projectName))
      .map(_ + "\n")

    val newScriptCreate = new File(s"$projectSpace/ScriptCreate.tcl")
    val writer = new PrintWriter(newScriptCreate)
    linesCreate.foreach(writer.write)
    writer.close()

    Process(s"quartus_sh -t ScriptCreate.tcl", new File(projectSpace)) !

    SpinalConfig(targetDirectory = projectSpace).generateVerilog(top)
  }

  def main(args: Array[String]): Unit = {
    CreateProject("/home/ltr/sysudas/project", "DasWhole", DasWhole())
  }
}

