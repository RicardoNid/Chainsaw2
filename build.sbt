name := "Chainsaw2"
//idePackagePrefix := Some("org.datenlord")

ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.11.12"
ThisBuild / organization := "org.example"

val spinalVersion = "1.7.3"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

lazy val mylib = (project in file("."))
  .settings(
    name := "SpinalTemplateSbt",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin)
  )

fork := true

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.3.0"

// graph theory
val jGraphTVersion = "1.4.0" // last version compatible with Java 1.8
libraryDependencies += "org.jgrapht" % "jgrapht-core" % jGraphTVersion
libraryDependencies += "org.jgrapht" % "jgrapht-ext" % jGraphTVersion

// numeric & matrix (numpy in scala)
libraryDependencies += "org.scalanlp" %% "breeze" % "1.0"

// finite field, poly ring
libraryDependencies += "cc.redberry" %% "rings.scaladsl" % "2.5.7"

// linear programming
libraryDependencies ++= Seq(
  "com.github.vagmcs" %% "optimus" % "3.2.4",
  "com.github.vagmcs" %% "optimus-solver-oj" % "3.2.4",
  "com.github.vagmcs" %% "optimus-solver-lp" % "3.2.4"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9"

libraryDependencies += "org.json4s" %% "json4s-jackson" % "4.0.3"

libraryDependencies += "com.regblanc" %% "scala-smtlib" % "0.2.2-12-g91e7214"

Compile / unmanagedJars += file("/home/ltr/matlab2020b/extern/engines/java/jar/engine.jar")
Compile / unmanagedJars += file("/opt/ibm/ILOG/CPLEX_Studio1210/cplex/lib/cplex.jar")
