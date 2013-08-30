import sbt._
import Keys._



object Toplevel extends Build
{
    lazy val commonSettings = Defaults.defaultSettings ++ Seq(
        scalaVersion    := "2.10.2",
        version         := "0.0.1",
        organization := "org.seacourt",
        scalacOptions   ++= Seq( "-deprecation", "-Xlint", "-optimize" ),
        resolvers       ++= Seq(
            Resolver.sonatypeRepo("snapshots"),
            "sourceforge jsi repository" at "http://sourceforge.net/projects/jsi/files/m2_repo",
            "maven2 dev repository" at "http://download.java.net/maven/2",
            "osgeo" at "http://download.osgeo.org/webdav/geotools"
        ),
        libraryDependencies ++= Seq(
            "com.twitter" %% "util-logging" % "6.3.6",
            "org.xeustechnologies" % "jtar" % "1.1",
            "com.twitter" %% "chill" % "0.2.3",
            "org.openstreetmap.osmosis" % "osmosis-pbf" % "0.43-RELEASE",
            "com.vividsolutions" % "jts" % "1.13",
            "net.sourceforge.jsi" % "jsi" % "1.0.0",
            "org.scalaz" % "scalaz-core_2.10" % "7.0.3"
        )
    )
    
    lazy val OSMlib = Project( id="OSMlib", base=file("OSMlib"),
        settings=commonSettings
    )
        
}

