resolvers += Resolver.url("HMRC Sbt Plugin Releases", url("https://dl.bintray.com/hmrc/sbt-plugin-releases"))(
  Resolver.ivyStylePatterns)
resolvers += "HMRC Releases" at "https://dl.bintray.com/hmrc/releases"

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.5.19")

addSbtPlugin("uk.gov.hmrc" % "sbt-auto-build" % "1.14.0")
addSbtPlugin("uk.gov.hmrc" % "sbt-git-versioning" % "1.16.0")
addSbtPlugin("uk.gov.hmrc" % "sbt-distributables" % "1.3.0")
addSbtPlugin("uk.gov.hmrc" % "sbt-artifactory" % "0.17.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")

addSbtPlugin("com.lucidchart" % "sbt-scalafmt" % "1.16")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.1")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.4")  // provides sbt command "dependencyUpdates"
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2")  // provides sbt command "dependencyTree"

addSbtPlugin("org.irundaia.sbt" % "sbt-sassify" % "1.4.8")