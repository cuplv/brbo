name := "brbo-impl"

version := "0.1"

scalaVersion := "2.12.12"

libraryDependencies += "org.checkerframework" % "checker" % "3.7.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.14.0"
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.14.0"

libraryDependencies += "commons-io" % "commons-io" % "2.5"

// Add tools.jar such that sbt can find it
unmanagedJars in Compile ~= {uj =>
  Seq(Attributed.blank(file(System.getProperty("java.home").dropRight(3)+"lib/tools.jar"))) ++ uj
}
// https://stackoverflow.com/questions/12409847/how-to-add-tools-jar-as-a-dynamic-dependency-in-sbt-is-it-possible/12508163