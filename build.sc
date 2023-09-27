// import Mill dependency
import mill._
import mill.define.Sources
import mill.modules.Util
import mill.scalalib.TestModule.ScalaTest
import scalalib._
// support BSP
import mill.bsp._

object darecreek extends RootModule with SbtModule { m =>
  override def millSourcePath = os.pwd
  override def scalaVersion = "2.12.13"
  override def scalacOptions = Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
    "-P:chiselplugin:genBundleElements"
  )
  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.5.5",
    ivy"edu.berkeley.cs::rocketchip:1.2.6",
  )
  override def scalacPluginIvyDeps = Agg(
    ivy"edu.berkeley.cs:::chisel3-plugin:3.5.5",
  )

  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def ivyDeps = m.ivyDeps() ++ Agg(
      ivy"edu.berkeley.cs::chiseltest:0.5.4"
    )

    def testFramework = "org.scalatest.tools.Framework"
  }
  // object test extends Tests with ScalaTest {
  //   override def ivyDeps = m.ivyDeps() ++ Agg(
  //     ivy"edu.berkeley.cs::chiseltest:0.5.4"
  //   )
  // }
}