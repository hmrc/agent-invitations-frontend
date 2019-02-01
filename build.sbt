import play.core.PlayVersion
import sbt.Tests.{Group, SubProcess}
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin._
import uk.gov.hmrc.SbtAutoBuildPlugin
import com.geirsson.coursiersmall.{Repository => R}

scalafixResolvers in ThisBuild += new R.Maven("https://artefacts.tax.service.gov.uk/artifactory/hmrc-releases")
scalafixDependencies in ThisBuild := Seq("uk.gov.hmrc" % "scalafix-rules_2.11" % "0.2.0")

lazy val scoverageSettings = {
  import scoverage.ScoverageKeys
  Seq(
    // Semicolon-separated list of regexs matching classes to exclude
    ScoverageKeys.coverageExcludedPackages := """uk\.gov\.hmrc\.BuildInfo;.*\.Routes;.*\.RoutesPrefix;.*Filters?;MicroserviceAuditConnector;Module;GraphiteStartUp;.*\.Reverse[^.]*""",
    ScoverageKeys.coverageMinimum := 80.00,
    ScoverageKeys.coverageFailOnMinimum := false,
    ScoverageKeys.coverageHighlighting := true,
    parallelExecution in Test := false
  )
}

lazy val compileDeps = Seq(
  ws,
  "uk.gov.hmrc" %% "bootstrap-play-25" % "4.8.0",
  "uk.gov.hmrc" %% "govuk-template" % "5.23.0",
  "uk.gov.hmrc" %% "play-ui" % "7.22.0",
  "uk.gov.hmrc" %% "agent-mtd-identifiers" % "0.13.0",
  "uk.gov.hmrc" %% "auth-client" % "2.6.0",
  "uk.gov.hmrc" %% "agent-kenshoo-monitoring" % "3.4.0",
  "uk.gov.hmrc" %% "play-partials" % "6.3.0",
  "de.threedimensions" %% "metrics-play" % "2.5.13",
  "uk.gov.hmrc" %% "http-caching-client" % "8.0.0"
)

def testDeps(scope: String) = Seq(
  "uk.gov.hmrc" %% "hmrctest" % "3.3.0" % scope,
  "com.github.tomakehurst" % "wiremock" % "2.21.0" % scope,
  "org.scalatest" %% "scalatest" % "3.0.5" % scope,
  "org.pegdown" % "pegdown" % "1.6.0" % scope,
  "org.jsoup" % "jsoup" % "1.11.3" % scope,
  "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
  "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.1" % scope,
  "org.mockito" % "mockito-core" % "2.23.4" % scope
)

lazy val root = (project in file("."))
  .settings(
    name := "agent-invitations-frontend",
    organization := "uk.gov.hmrc",
    scalaVersion := "2.11.11",
    majorVersion := 0,
    PlayKeys.playDefaultPort := 9448,
    resolvers := Seq(
      Resolver.bintrayRepo("hmrc", "releases"),
      Resolver.bintrayRepo("hmrc", "release-candidates"),
      Resolver.typesafeRepo("releases"),
      Resolver.jcenterRepo
    ),
    libraryDependencies ++= compileDeps ++ testDeps("test") ++ testDeps("it"),
    routesImport += "uk.gov.hmrc.agentinvitationsfrontend.binders.UrlBinders._",
    publishingSettings,
    scoverageSettings,
    unmanagedResourceDirectories in Compile += baseDirectory.value / "resources",
    scalafmtOnCompile in Compile := true,
    scalafmtOnCompile in Test := true
  )
  .configs(IntegrationTest)
  .settings(
    Keys.fork in IntegrationTest := false,
    Defaults.itSettings,
    unmanagedSourceDirectories in IntegrationTest += baseDirectory(_ / "it").value,
    parallelExecution in IntegrationTest := false,
    testGrouping in IntegrationTest := oneForkedJvmPerTest((definedTests in IntegrationTest).value),
    scalafmtOnCompile in IntegrationTest := true
  )
  .settings(addCompilerPlugin(scalafixSemanticdb))
  .settings(
    scalacOptions ++= List(
      "-Yrangepos",
      "-Xplugin-require:semanticdb",
      "-P:semanticdb:synthetics:on"
    )
  )
  .enablePlugins(PlayScala, SbtAutoBuildPlugin, SbtGitVersioning, SbtDistributablesPlugin, SbtArtifactory)

inConfig(IntegrationTest)(scalafmtCoreSettings)

def oneForkedJvmPerTest(tests: Seq[TestDefinition]) = {
  tests.map { test =>
    new Group(test.name, Seq(test), SubProcess(ForkOptions(runJVMOptions = Seq(s"-Dtest.name=${test.name}"))))
  }
}

