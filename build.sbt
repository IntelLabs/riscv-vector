scalaVersion := "2.12.13"

val chiselVersion = "3.5.5"

scalacOptions ++= Seq(
  "-feature",
  "-language:reflectiveCalls",
  "-deprecation",
  "-Xcheckinit",
  "-P:chiselplugin:genBundleElements",
)

addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % chiselVersion
libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.5.4"
libraryDependencies += "edu.berkeley.cs" %% "rocketchip" % "1.2.6"
// libraryDependencies += "io.github.chiselverify" % "chiselverify" % "0.2.0"
// libraryDependencies += "org.apache.poi" % "poi" % "4.1.1"
// libraryDependencies += "org.apache.poi" % "poi-ooxml" % "4.1.1"