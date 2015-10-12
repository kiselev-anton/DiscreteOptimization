resolvers ++= Seq(
  Resolver.bintrayRepo("mtrupkin", "sbt-plugins"),
  "Alfresco Repo" at "https://maven.alfresco.com/nexus/content/groups/public/",
  "Simulation @ TU Delft" at "http://simulation.tudelft.nl/maven/"
)

addSbtPlugin("org.trupkin" % "sbt-launch4j" % "0.0.6")