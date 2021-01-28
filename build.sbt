scalaVersion := "2.13.3"

name := "Scala ray tracer"

scalacOptions ++= Seq("-deprecation", "-feature")

val AkkaVersion = "2.6.11"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.9.0",
  "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion
)
