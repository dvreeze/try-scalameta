# Creates a classpath string that can be fed to scalac (maybe after removing the top level dependency itself, if applicable)
cs fetch --classpath -E org.scala-lang:scala-library eu.cdevreeze.tryscalameta:tryscalameta_2.13:0.1.0
