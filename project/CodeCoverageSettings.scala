import sbt.Keys.parallelExecution
import sbt.Test
import scoverage.ScoverageKeys

object CodeCoverageSettings {

  lazy val scoverageSettings = {

    Seq(
      // Semicolon-separated list of regexs matching classes to exclude
      ScoverageKeys.coverageExcludedPackages := """uk\.gov\.hmrc\.BuildInfo;.*\.Routes;.*\.RoutesPrefix;.*Filters?;MicroserviceAuditConnector;Module;GraphiteStartUp;.*\.Reverse[^.]*""",
      ScoverageKeys.coverageMinimumStmtTotal := 00.00,
      ScoverageKeys.coverageFailOnMinimum := true,
      ScoverageKeys.coverageHighlighting := true,
      Test / parallelExecution := false
    )
  }
}
