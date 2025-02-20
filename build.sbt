scalaVersion := "3.3.5"

name := "Scala ray tracer"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.playframework" %% "play-json" % "3.0.4",
  "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
  "com.lihaoyi" %% "pprint" % "0.9.0"
)
