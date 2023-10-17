resolvers += MavenRepository("HMRC-open-artefacts-maven2", "https://open.artefacts.tax.service.gov.uk/maven2")
resolvers += Resolver.url("HMRC-open-artefacts-ivy2", url("https://open.artefacts.tax.service.gov.uk/ivy2"))(Resolver.ivyStylePatterns)
resolvers += Resolver.typesafeRepo("releases")

addSbtPlugin("uk.gov.hmrc"          % "sbt-auto-build"        % "3.14.0")
addSbtPlugin("uk.gov.hmrc"          % "sbt-distributables"    % "2.2.0")
addSbtPlugin("com.typesafe.play"    % "sbt-plugin"            % "2.8.20")
addSbtPlugin("com.typesafe.sbt"     % "sbt-twirl"             % "1.5.1")
addSbtPlugin("org.scoverage"        % "sbt-scoverage"         % "1.9.3")
addSbtPlugin("com.lucidchart"       % "sbt-scalafmt"          % "1.16")
addSbtPlugin("com.timushev.sbt"     % "sbt-updates"           % "0.3.4")  // provides sbt command "dependencyUpdates"
addSbtPlugin("io.github.irundaia"   % "sbt-sassify"           % "1.5.2")
addDependencyTreePlugin
