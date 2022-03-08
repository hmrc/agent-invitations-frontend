resolvers += MavenRepository("HMRC-open-artefacts-maven2", "https://open.artefacts.tax.service.gov.uk/maven2")
resolvers += Resolver.url("HMRC-open-artefacts-ivy", url("https://open.artefacts.tax.service.gov.uk/ivy2"))(Resolver.ivyStylePatterns)

resolvers += Resolver.typesafeRepo("releases")

addSbtPlugin("com.typesafe.sbt"  % "sbt-twirl"            % "1.5.1")
addSbtPlugin("com.typesafe.play" % "sbt-plugin"           % "2.8.7")

addSbtPlugin("uk.gov.hmrc"       % "sbt-auto-build"       % "3.6.0")
addSbtPlugin("uk.gov.hmrc"       % "sbt-distributables"   % "2.1.0")

addSbtPlugin("org.scoverage"     % "sbt-scoverage"        % "1.6.1")
addSbtPlugin("com.lucidchart"    % "sbt-scalafmt"         % "1.16")
addSbtPlugin("ch.epfl.scala"     % "sbt-scalafix"         % "0.9.19")
addSbtPlugin("com.timushev.sbt"  % "sbt-updates"          % "0.3.4")  // provides sbt command "dependencyUpdates"
addDependencyTreePlugin
addSbtPlugin("org.irundaia.sbt"  % "sbt-sassify"          % "1.5.1")