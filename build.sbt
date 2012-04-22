
name := "CollFilter"

scalaVersion := "2.10.0-M2"
//scalaVersion := "2.9.2"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-optimise")

//Akka

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "se.scalablesolutions.akka" % "akka-actor" % "1.3.1"
//libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.1"
