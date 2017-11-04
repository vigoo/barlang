name := "barlang"

version := "1.0"

scalaVersion := "2.12.3"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "org.typelevel" %% "cats-core" % "1.0.0-MF",
  "org.atnos" %% "eff" % "5.0.0+"
)

scalacOptions += "-Ypartial-unification"
