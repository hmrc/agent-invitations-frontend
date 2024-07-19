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
    scalaVersion := "2.12.19",
    majorVersion := 1,
    PlayKeys.playDefaultPort := 9448,
    resolvers ++= Seq(
      Resolver.typesafeRepo("releases"),
      "HMRC-open-artefacts-maven" at "https://open.artefacts.tax.service.gov.uk/maven2",
      Resolver.url("HMRC-open-artefacts-ivy", url("https://open.artefacts.tax.service.gov.uk/ivy2"))(Resolver.ivyStylePatterns),
    ),
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test,
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
      "-Ypatmat-exhaust-depth", "40",
      "-Wconf:src=target/.*:s", // silence warnings from compiled files
      "-Wconf:src=*routes:s", // silence warnings from routes files
      "-Wconf:src=*html:w", // silence html warnings as they are wrong
    )
  )
  .enablePlugins(PlayScala, SbtDistributablesPlugin)
  .disablePlugins(JUnitXmlReportPlugin)
