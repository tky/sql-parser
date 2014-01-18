import sbt._
import Keys._

object MongoSqlConverter extends Build {

  lazy val _version = "0.0.1"

  val baseDependency = Seq(
    "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
    "org.apache.commons" % "commons-lang3" % "3.1"
  )

  lazy val main = Project (
    id = "mongo-sql-converter",
    base = file ("."),
    settings = Defaults.defaultSettings ++ Seq (
      name := "mongo-sql-converter",
      organization := "com.github.tky",
      version := _version,
      scalaVersion := "2.10.2",
      resolvers ++= Seq(
        "ATILIKA dependencies" at "http://www.atilika.org/nexus/content/repositories/atilika"
      ),
      libraryDependencies ++= baseDependency,
      scalacOptions ++= Seq("-deprecation", "-language:_")
    )
  )
}
