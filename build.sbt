name := "OSMcrunch"

organization := "org.seacourt"

version := "0.0.1"

scalaVersion := "2.10.2"



resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += "sourceforge jsi repository" at "http://sourceforge.net/projects/jsi/files/m2_repo"

resolvers += "maven2 dev repository" at "http://download.java.net/maven/2"

//resolvers += "spring source" at "http://repository.springsource.com/maven/bundles/external"

resolvers += "osgeo" at "http://download.osgeo.org/webdav/geotools"

//libraryDependencies += "javax.media.jai" % "com.springsource.javax.media.jai.core" % "1.1.3"

//libraryDependencies += "javax.media" % "jai_core" % "1.1.3"

//libraryDependencies ++= {
//    val geoToolsVersion = "8.0-M4"
//    Seq(
//        "org.geotools" % "gt-main" % geoToolsVersion,
//        "org.geotools" % "gt-coverage" % geoToolsVersion,
//        "org.geotools" % "gt-geotiff" % geoToolsVersion )
//}

libraryDependencies += "com.twitter" %% "util-logging" % "6.3.6"

libraryDependencies += "org.xeustechnologies" % "jtar" % "1.1"

libraryDependencies += "com.twitter" %% "chill" % "0.2.3"

//libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.8.0-SNAPSHOT"

libraryDependencies += "org.openstreetmap.osmosis" % "osmosis-pbf" % "0.43-RELEASE"

libraryDependencies += "com.vividsolutions" % "jts" % "1.13"

libraryDependencies += "net.sourceforge.jsi" % "jsi" % "1.0.0"

libraryDependencies += "org.scalaz" % "scalaz-core_2.10" % "7.0.3"

scalacOptions ++= Seq( "-deprecation", "-Xlint", "-optimize" )



