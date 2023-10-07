// import Mill dependency
import mill._
import mill.define.Sources
import mill.modules.Util
import mill.scalalib.TestModule.ScalaTest
import scalalib._
// support BSP
import mill.bsp._

object darecreek extends SbtModule { m =>
  override def millSourcePath = os.pwd
  override def scalaVersion = "2.13.10"
  override def scalacOptions = Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
  )
  override def ivyDeps = Agg(
    ivy"org.chipsalliance::chisel:6.0.0-M3",
  )
  override def scalacPluginIvyDeps = Agg(
    ivy"org.chipsalliance:::chisel-plugin:6.0.0-M3",
  )

  // object test extends SbtModuleTests with TestModule.ScalaTest {
  //   override def ivyDeps = m.ivyDeps() ++ Agg(
  //     ivy"edu.berkeley.cs::chiseltest:0.5.4"
  //   )

  //   def testFramework = "org.scalatest.tools.Framework"
  // }
}
