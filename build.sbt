scalaVersion := "3.3.1"

name := "Scala ray tracer"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.playframework" %% "play-json" % "3.0.1",
  "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
  "com.lihaoyi" %% "pprint" % "0.8.1"
)
