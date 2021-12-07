import play.core.PlayVersion
import uk.gov.hmrc.SbtAutoBuildPlugin
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin._

lazy val scoverageSettings = {
  import scoverage.ScoverageKeys
  Seq(
    // Semicolon-separated list of regexs matching classes to exclude
    ScoverageKeys.coverageExcludedPackages := """uk\.gov\.hmrc\.BuildInfo;.*\.Routes;.*\.RoutesPrefix;.*Filters?;MicroserviceAuditConnector;Module;GraphiteStartUp;.*\.Reverse[^.]*""",
    ScoverageKeys.coverageMinimum := 80.00,
    ScoverageKeys.coverageFailOnMinimum := true,
    ScoverageKeys.coverageHighlighting := true,
    parallelExecution in Test := false
  )
}

lazy val compileDeps = Seq(
  "uk.gov.hmrc"       %% "bootstrap-frontend-play-28" % "5.16.0",
  "uk.gov.hmrc"       %% "play-frontend-hmrc"         % "1.22.0-play-28",
  "uk.gov.hmrc"       %% "play-fsm"                   % "0.87.0-play-28",
  "uk.gov.hmrc"       %% "govuk-template"             % "5.70.0-play-28",
  "uk.gov.hmrc"       %% "play-ui"                    % "9.7.0-play-28",
  "uk.gov.hmrc"       %% "agent-mtd-identifiers"      % "0.28.0-play-27",
  "uk.gov.hmrc"       %% "agent-kenshoo-monitoring"   % "4.8.0-play-28",
  "uk.gov.hmrc"       %% "play-partials"              % "8.2.0-play-28",
  "uk.gov.hmrc"       %% "mongo-caching"              % "7.0.0-play-28",
  "uk.gov.hmrc"       %% "play-language"              % "5.1.0-play-28",
  "com.typesafe.play" %% "play-json-joda"             % "2.9.2"
)

def testDeps(scope: String) = Seq(
  "org.scalatestplus.play" %% "scalatestplus-play"  % "5.1.0"         % scope,
  "org.scalatestplus"      %% "mockito-3-12"        % "3.2.10.0"      % scope,
  "com.github.tomakehurst"  % "wiremock-jre8"       % "2.26.2"        % scope,
  "org.jsoup"               % "jsoup"               % "1.14.2"        % scope,
  "uk.gov.hmrc"             %% "reactivemongo-test" % "5.0.0-play-28" % scope,
  "com.vladsch.flexmark"    % "flexmark-all"        % "0.35.10"       % scope
)

lazy val root = (project in file("."))
  .settings(
    name := "agent-invitations-frontend",
    organization := "uk.gov.hmrc",
    scalaVersion := "2.12.12",
    majorVersion := 0,
    PlayKeys.playDefaultPort := 9448,
    resolvers := Seq(
      Resolver.typesafeRepo("releases"),
    ),
    resolvers += "HMRC-open-artefacts-maven" at "https://open.artefacts.tax.service.gov.uk/maven2",
    resolvers += "HMRC-local-artefacts-maven" at "https://artefacts.tax.service.gov.uk/artifactory/hmrc-releases-local",
    resolvers += Resolver.url("HMRC-open-artefacts-ivy", url("https://open.artefacts.tax.service.gov.uk/ivy2"))(Resolver.ivyStylePatterns),
    libraryDependencies ++= compileDeps ++ testDeps("test") ++ testDeps("it"),
    libraryDependencies ++= Seq(
      compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.7.0" cross CrossVersion.full),
      "com.github.ghik" % "silencer-lib" % "1.7.0" % Provided cross CrossVersion.full
    ),
    routesImport += "uk.gov.hmrc.agentinvitationsfrontend.binders.UrlBinders._",
    publishingSettings,
    scoverageSettings,
    unmanagedResourceDirectories in Compile += baseDirectory.value / "resources",
    scalafmtOnCompile in Compile := true,
    scalafmtOnCompile in Test := true,
    routesGenerator := InjectedRoutesGenerator
  )
  .configs(IntegrationTest)
  .settings(
    Keys.fork in IntegrationTest := false,
    Defaults.itSettings,
    unmanagedSourceDirectories in IntegrationTest += baseDirectory(_ / "it").value,
    parallelExecution in IntegrationTest := false,
    scalafmtOnCompile in IntegrationTest := true
  )
  .settings(addCompilerPlugin(scalafixSemanticdb))
  .settings(
    scalacOptions ++= List(
      "-Yrangepos",
      "-Xplugin-require:semanticdb",
      "-P:semanticdb:synthetics:on",
      "-Xlint:-missing-interpolator,_",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:implicitConversions",
      "-P:silencer:pathFilters=views;routes;TestStorage"
    )
  )
  .enablePlugins(PlayScala, SbtDistributablesPlugin)
  .disablePlugins(JUnitXmlReportPlugin)

inConfig(IntegrationTest)(scalafmtCoreSettings)
