name := "barlang"

version := "1.0"

scalaVersion := "2.12.8"

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
  "org.typelevel" %% "cats-core" % "1.6.0",
  "org.atnos" %% "eff" % "5.5.0",
  "io.github.vigoo" %% "simpp" % "0.2-SNAPSHOT",
  "io.github.vigoo" %% "clipp" % "0.1-SNAPSHOT",

  "org.specs2" %% "specs2-core" % "4.4.1" % "test"
)

scalacOptions += "-Ypartial-unification"

coverageEnabled in(Test, compile) := true
coverageEnabled in(Compile, compile) := false
