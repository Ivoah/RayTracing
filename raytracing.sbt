scalaVersion := "2.13.3"

name := "Scala ray tracer"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "com.typesafe.play" %%% "play-json" % "2.9.0"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0"

enablePlugins(ScalaJSPlugin)

name := "Scala.js ray tracer"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true
