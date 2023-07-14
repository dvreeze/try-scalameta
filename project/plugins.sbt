
// For a list of well-known plugins, see https://www.scala-sbt.org/1.x/docs/Community-Plugins.html.

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.2")
// addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.7.3")

// See https://github.com/scalameta/sbt-scalafmt
// Tasks: scalafmt
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.0")

// See https://github.com/albuch/sbt-dependency-check (the plugin checks potential security vulnerabilities)
// Tasks: dependencyCheck, dependencyCheckAggregate, etc.
addSbtPlugin("net.vonbuchholtz" % "sbt-dependency-check" % "5.1.0")

// See https://github.com/rtimush/sbt-updates
// Tasks: dependencyUpdates, dependencyUpdatesReport
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.3")

// See https://github.com/cb372/sbt-explicit-dependencies (like maven-dependency-plugin:analyze)
// Tasks: undeclaredCompileDependencies, undeclaredCompileDependenciesTest, unusedCompileDependencies etc.
addSbtPlugin("com.github.cb372" % "sbt-explicit-dependencies" % "0.3.1")

// See https://github.com/scalacenter/sbt-missinglink
addSbtPlugin("ch.epfl.scala" % "sbt-missinglink" % "0.3.5")

// See https://github.com/sbt/sbt-duplicates-finder (finds duplicates at level of classes etc.)
// Should detect Saxon-HE and Saxon-EE together on classpath
// Tasks: checkDuplicates, checkDuplicatesTest
addSbtPlugin("com.github.sbt" % "sbt-duplicates-finder" % "1.1.0")

// See https://scalacenter.github.io/scalafix/docs/users/installation.html
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.11.0")
