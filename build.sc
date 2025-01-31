
import mill._
import scalalib._
import scalafmt._
// import $file.diff.build
import $file.`rocket-chip`.dependencies.hardfloat.common
import $file.`rocket-chip`.dependencies.cde.common
import $file.`rocket-chip`.dependencies.diplomacy.common
import $file.`rocket-chip`.common
import $file.difftest.common
import $packages._
// val chiselVersion = "6.6.0"
// val defaultScalaVersion = "2.13.14"
val chiselVersion = "7.0.0-M2"
val defaultScalaVersion = "2.13.14"
val pwd = os.Path(sys.env("MILL_WORKSPACE_ROOT"))

object v {
  def chiselIvy: Option[Dep] = Some(ivy"org.chipsalliance::chisel:${chiselVersion}")
  def chiselPluginIvy: Option[Dep] = Some(ivy"org.chipsalliance:::chisel-plugin:${chiselVersion}")
  // def chipNeed :  Option[Dep] = Some(ivy"org.chipsalliance:::diplomacy:${chiselVersion}")
}

trait HasThisChisel extends SbtModule {
  def chiselModule: Option[ScalaModule] = None
  def chiselPluginJar: T[Option[PathRef]] = None
  def chiselIvy: Option[Dep] = v.chiselIvy
  def chiselPluginIvy: Option[Dep] = v.chiselPluginIvy
  override def scalaVersion = defaultScalaVersion
  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")
  override def ivyDeps = super.ivyDeps() ++ Agg(chiselIvy.get)
  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(chiselPluginIvy.get)
}

object rocketchip extends RocketChip
trait RocketChip extends $file.`rocket-chip`.common.RocketChipModule with HasThisChisel {
  def scalaVersion: T[String] = T(defaultScalaVersion)
  override def millSourcePath = pwd / "rocket-chip"
  def dependencyPath = millSourcePath / "dependencies"
  def macrosModule = macros
  def hardfloatModule = hardfloat
  def cdeModule = cde
  def diplomacyModule = diplomacy
  // def diplomacyIvy = None
  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.5.4"
  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.6"

  object macros extends Macros
  trait Macros extends $file.`rocket-chip`.common.MacrosModule with SbtModule {
    def scalaVersion: T[String] = T(defaultScalaVersion)
    def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${defaultScalaVersion}"
  }

  object hardfloat extends Hardfloat
  trait Hardfloat extends $file.`rocket-chip`.dependencies.hardfloat.common.HardfloatModule with HasThisChisel {
    def scalaVersion: T[String] = T(defaultScalaVersion)
    override def millSourcePath = dependencyPath / "hardfloat" / "hardfloat"
  }

  object cde extends CDE
  trait CDE extends $file.`rocket-chip`.dependencies.cde.common.CDEModule with ScalaModule {
    def scalaVersion: T[String] = T(defaultScalaVersion)
    override def millSourcePath = dependencyPath / "cde" / "cde"
  }

  object diplomacy extends Diplomacy
  trait Diplomacy extends $file.`rocket-chip`.dependencies.diplomacy.common.DiplomacyModule {
    def scalaVersion: T[String] = T(defaultScalaVersion)
    override def millSourcePath = dependencyPath / "diplomacy" / "diplomacy"

    def chiselModule: Option[ScalaModule] = None
    def chiselPluginJar: T[Option[PathRef]] = None
    def chiselIvy: Option[Dep] = v.chiselIvy
    def chiselPluginIvy: Option[Dep] = v.chiselPluginIvy

    def cdeModule = cde
    def sourcecodeIvy = ivy"com.lihaoyi::sourcecode:0.3.1"
  }
}
// object difftest extends $file.difftest.common.DiffTestModule {
//   def crossValue: String = "chisel"
//   // def scalaVersion: T[String] = T(defaultScalaVersion)
//   override def millSourcePath = os.pwd / "difftest"
  
// }

object difftest extends Cross[$file.difftest.common.CommonDiffTest]("chisel") {
  override def millSourcePath = os.pwd / "difftest"
}

trait mymodule extends ScalaModule {
  def rocketModule: ScalaModule
  def difftestModule: ScalaModule
  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketModule,difftestModule
  )
}

trait mycore extends mymodule with HasThisChisel  {
  override def millSourcePath = pwd
  def rocketModule = rocketchip
  def difftestModule = difftest("chisel")
  override def sources = T.sources {
    super.sources() ++ Seq(PathRef(millSourcePath /"src"))
    // ++Seq(PathRef(millSourcePath /"difftest"/"src"))
  }
}
object playground extends mycore with ScalafmtModule {
  
}