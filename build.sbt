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
    Test / parallelExecution := false
  )
}

lazy val compileDeps = Seq(
  "uk.gov.hmrc"       %% "bootstrap-frontend-play-28" % "5.20.0",
  "uk.gov.hmrc"       %% "play-frontend-hmrc"         % "3.5.0-play-28",
  "uk.gov.hmrc"       %% "play-fsm"                   % "0.89.0-play-28",
  "uk.gov.hmrc"       %% "agent-mtd-identifiers"      % "0.35.0-play-28",
  "uk.gov.hmrc"       %% "agent-kenshoo-monitoring"   % "4.8.0-play-28",
  "uk.gov.hmrc"       %% "play-partials"              % "8.3.0-play-28",
  "uk.gov.hmrc"       %% "mongo-caching"              % "7.1.0-play-28",
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

TwirlKeys.templateImports ++= Seq(
  "uk.gov.hmrc.agentinvitationsfrontend.views.html.components._",
  "uk.gov.hmrc.govukfrontend.views.html.components._",
  "uk.gov.hmrc.hmrcfrontend.views.html.components._",
)

lazy val root = (project in file("."))
  .settings(
    name := "agent-invitations-frontend",
    organization := "uk.gov.hmrc",
    scalaVersion := "2.12.15",
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
      compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.7.7" cross CrossVersion.full),
      "com.github.ghik" % "silencer-lib" % "1.7.7" % Provided cross CrossVersion.full
    ),
    routesImport += "uk.gov.hmrc.agentinvitationsfrontend.binders.UrlBinders._",
    publishingSettings,
    scoverageSettings,
    Compile / unmanagedResourceDirectories += baseDirectory.value / "resources",
    Compile / scalafmtOnCompile := true,
    Test / scalafmtOnCompile := true,
    routesGenerator := InjectedRoutesGenerator
  )
  .configs(IntegrationTest)
  .settings(
    IntegrationTest / Keys.fork := false,
    Defaults.itSettings,
    IntegrationTest / unmanagedSourceDirectories += baseDirectory(_ / "it").value,
    IntegrationTest / parallelExecution := false
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
