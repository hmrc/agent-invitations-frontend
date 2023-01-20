import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin._
import AppDependencies._
import CodeCoverageSettings._


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
      "HMRC-open-artefacts-maven" at "https://open.artefacts.tax.service.gov.uk/maven2",
      Resolver.url("HMRC-open-artefacts-ivy", url("https://open.artefacts.tax.service.gov.uk/ivy2"))(Resolver.ivyStylePatterns),
    ),
    libraryDependencies ++= compileDeps ++ testDeps,
    libraryDependencies ++= Seq(
      compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.7.7" cross CrossVersion.full),
      "com.github.ghik" % "silencer-lib" % "1.7.7" % Provided cross CrossVersion.full
    ),
    routesImport += "uk.gov.hmrc.agentinvitationsfrontend.binders.UrlBinders._",
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
  .settings(
    scalacOptions ++= List(
      "-Yrangepos",
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
