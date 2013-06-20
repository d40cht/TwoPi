name := "OSMcrunch"

organization := "org.seacourt"

version := "0.0.1"

scalaVersion := "2.10.2"

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += "sourceforge jsi repository" at "http://sourceforge.net/projects/jsi/files/m2_repo"

libraryDependencies += "com.twitter" %% "util-logging" % "6.3.6"

libraryDependencies += "com.twitter" %% "chill" % "0.2.3"

//libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.8.0-SNAPSHOT"

libraryDependencies += "org.openstreetmap.osmosis" % "osmosis-pbf" % "0.43-RELEASE"

libraryDependencies += "com.vividsolutions" % "jts" % "1.13"

libraryDependencies += "net.sourceforge.jsi" % "jsi" % "1.0.0"

scalacOptions ++= Seq( "-deprecation", "-Xlint", "-optimize" )



