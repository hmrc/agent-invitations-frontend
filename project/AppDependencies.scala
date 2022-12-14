import sbt._

object AppDependencies {

  private val mongoVersion: String = "0.74.0"

  lazy val compileDeps = Seq(
    "uk.gov.hmrc"       %% "bootstrap-frontend-play-28" % "7.12.0",
    "uk.gov.hmrc"       %% "play-frontend-hmrc"         % "4.1.0-play-28",
    "uk.gov.hmrc"       %% "play-fsm"                   % "0.89.0-play-28",
    "uk.gov.hmrc"       %% "agent-mtd-identifiers"      % "0.51.0-play-28",
    "uk.gov.hmrc"       %% "agent-kenshoo-monitoring"   % "4.8.0-play-28",
    "uk.gov.hmrc"       %% "play-partials"              % "8.3.0-play-28",
    "uk.gov.hmrc.mongo" %% "hmrc-mongo-play-28"         % mongoVersion
  )

  def testDeps(scope: String) = Seq(
    "org.scalatestplus.play"  %% "scalatestplus-play"       % "5.1.0"         % scope,
    "org.scalatestplus"       %% "mockito-3-12"             % "3.2.10.0"      % scope,
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-test-play-28"  % mongoVersion    % scope,
    "com.github.tomakehurst"  % "wiremock-jre8"             % "2.26.2"        % scope,
    "org.jsoup"               % "jsoup"                     % "1.14.2"        % scope,
    "com.vladsch.flexmark"    % "flexmark-all"              % "0.36.8"        % scope
  )

}
