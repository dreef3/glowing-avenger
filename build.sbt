name := "glowing-avenger"

version := "0.0.1"

scalaVersion := "2.10.3"

javaHome := Some(file("/usr/lib/jvm/java-7-oracle/"))

ideaExcludeFolders += ".idea"

ideaExcludeFolders += ".idea_modules"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

libraryDependencies += "org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5"

libraryDependencies += "org.jgrapht" % "jgrapht-core" % "0.9.0"

libraryDependencies += "org.jgrapht" % "jgrapht-ext" % "0.9.0"

libraryDependencies += "org.scala-lang" % "scala-swing" % scalaVersion.value
