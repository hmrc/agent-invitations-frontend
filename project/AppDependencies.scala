import sbt._

object AppDependencies {

  private val mongoVersion: String = "1.4.0"
  private val bootstrapVer: String = "7.23.0"

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"        %% "bootstrap-frontend-play-28" % bootstrapVer,
    "uk.gov.hmrc"        %% "play-frontend-hmrc-play-28" % "8.5.0",
    "uk.gov.hmrc"        %% "play-fsm"                   % "0.89.0-play-28",
    "uk.gov.hmrc"        %% "agent-mtd-identifiers"      % "1.15.0",
    "uk.gov.hmrc"        %% "agent-kenshoo-monitoring"   % "5.4.0",
    "uk.gov.hmrc"        %% "play-partials"              % "8.4.0-play-28",
    "uk.gov.hmrc.mongo"  %% "hmrc-mongo-play-28"         % mongoVersion,
    "com.github.blemale" %% "scaffeine"                  % "4.0.1",
  )

  val test: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"             %% "bootstrap-test-play-28"   % bootstrapVer    % "test, it",
    "org.scalatestplus.play"  %% "scalatestplus-play"       % "5.1.0"         % "test, it",
    "org.scalatestplus"       %% "mockito-3-12"             % "3.2.10.0"      % "test, it",
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-test-play-28"  % mongoVersion    % "test, it",
    "com.github.tomakehurst"  % "wiremock-jre8"             % "2.26.2"        % "test, it",
    "org.jsoup"               % "jsoup"                     % "1.15.4"        % "test, it",
    "com.vladsch.flexmark"    % "flexmark-all"              % "0.36.8"        % "test, it"
  )

}
