name := "OSMcrunch"

organization := "org.seacourt"

version := "0.0.1"

scalaVersion := "2.10.2"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "com.twitter" %% "util-logging" % "6.3.6"

libraryDependencies += "com.twitter" %% "chill" % "0.2.3"

//libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.8.0-SNAPSHOT"

libraryDependencies += "org.openstreetmap.osmosis" % "osmosis-pbf" % "0.43-RELEASE"

scalacOptions ++= Seq( "-deprecation", "-Xlint", "-optimize" )



