name := "DiscreteOptimization"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.assembla.scala-incubator" %% "graph-core" % "1.9.4",
  "org.scalatest" % "scalatest_2.11" % "latest.integration" % "test"
)

/**
 *  Here we should choose the task to execute in JAR:
 *   - task1
 *   - task2
 *   - task3
 *   - task4 (not ready)
 *   - task5 (not available yet)
 */
mainClass in assembly := Some("task3.Main")

mainClass in buildLauncher := Some("task1.Main")

assemblyJarName in assembly := "DiscreteOptimization3.jar"