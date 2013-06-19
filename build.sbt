name := "OSMcrunch"

organization := "org.seacourt"

version := "0.0.1"

scalaVersion := "2.9.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1"

libraryDependencies += "com.twitter" % "util-logging_2.9.2" % "6.3.6"

libraryDependencies += "org.openstreetmap.osmosis" % "osmosis-pbf" % "0.43-RELEASE"

scalacOptions ++= Seq( "-deprecation", "-Xlint", "-optimize" )



