import sbt._
import Keys._
import org.scalatra.sbt._
import org.scalatra.sbt.PluginKeys._
import com.mojolly.scalate.ScalatePlugin._
import ScalateKeys._


object Toplevel extends Build
{
    lazy val scalatraVersion = "2.2.1"
    
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
    
    lazy val routeSite = Project( id="routeSite", base=file("routeSite"),
        settings=commonSettings ++ ScalatraPlugin.scalatraWithJRebel ++ scalateSettings ++ Seq(
            libraryDependencies ++= Seq(
                "org.scalaj" %% "scalaj-http" % "0.3.9" exclude("junit", "junit"),
                "net.liftweb" %% "lift-json" % "2.5.1",
                "org.scalatra" %% "scalatra" % scalatraVersion,
                "org.scalatra" %% "scalatra-scalate" % scalatraVersion,
                "org.scalatra" %% "scalatra-specs2" % scalatraVersion % "test",
                "ch.qos.logback" % "logback-classic" % "1.0.6" % "runtime",
                "org.eclipse.jetty" % "jetty-webapp" % "8.1.8.v20121106" % "container",
                "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container;provided;test" artifacts (Artifact("javax.servlet", "jar", "jar"))
            ),
            scalateTemplateConfig in Compile <<= (sourceDirectory in Compile){ base =>
                Seq(
                    TemplateConfig(
                        base / "webapp" / "WEB-INF" / "templates",
                        Seq.empty, /* default imports should be added here */
                        Seq(
                            Binding("context", "_root_.org.scalatra.scalate.ScalatraRenderContext", importMembers = true, isImplicit = true)
                        ), /* add extra bindings here */
                        Some("templates")
                    )
                )
            }
        )
    )
    .dependsOn( OSMlib )
        
}

