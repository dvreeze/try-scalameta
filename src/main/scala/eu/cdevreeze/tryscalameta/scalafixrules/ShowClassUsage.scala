/*
 * Copyright 2022-2022 Chris de Vreeze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eu.cdevreeze.tryscalameta.scalafixrules

import java.nio.file.Path
import java.nio.file.Paths

import scala.meta._
import scala.util.chaining.scalaUtilChainingOps

import metaconfig.ConfDecoder
import metaconfig.Configured
import metaconfig.generic.Surface
import scalafix.v1._

/**
 * Shows usage of public classes/traits/objects whose symbols are passed as configuration data. Only exact matches are
 * returned, without following any inheritance chains.
 *
 * This "rule" only depends on the Scala standard library and on Scalafix (and therefore Scalameta) and nothing else, so
 * this rule can easily be run from its source path against sbt or Maven projects. For example: "scalafix
 * --rules=file:/path/to/ShowClassUsage.scala --config=/path/to/scalafix-config-file --classpath=./target/classes"
 *
 * The classpath configuration option should point to the parent directory of a "META-INF/semanticdb/src/main/scala"
 * directory, where the "*.scala.semanticdb" files live.
 *
 * With "-f" or "--files" (once or more) specific (relative) Scala source directories can be chosen.
 *
 * See https://scalacenter.github.io/scalafix/docs/users/installation.html#command-line for more information.
 *
 * To run this semantic rule, first make sure that the Scala compiler has emitted "*.semanticdb" files. See for example
 * https://scalacenter.github.io/scalafix/docs/users/installation.html for sbt, and
 * https://github.com/evis/scalafix-maven-plugin for Maven. Only the semanticdb-scalac compiler plugin configuration is
 * needed, if scalafix is run directly from the command line. Just don't forget to compile first before running the
 * semantic "rule", or else it misses the needed "*.semanticdb" files as input.
 *
 * @author
 *   Chris de Vreeze
 */
final class ShowClassUsage(val config: UsedClassConfig) extends SemanticRule("ShowClassUsage") {

  def this() = this(UsedClassConfig.default)

  override def withConfiguration(config: Configuration): Configured[Rule] =
    config.conf
      .getOrElse("ShowClassUsage")(this.config)
      .map(newConfig => new ShowClassUsage(newConfig))

  override def fix(implicit doc: SemanticDocument): Patch = {
    if (config.isEmpty) {
      println("Missing classSymbols (symbols of classes/traits/objects). Doing nothing.")
      Patch.empty
    } else {
      // Replacing ".." in HOCON by "#"
      val classSymbols: Set[Symbol] =
        config.classSymbols.ensuring(_.nonEmpty).map(_.replace("..", "#")).map(Symbol.apply).toSet

      classSymbols.foreach(checkClassSymbol)

      val symbolMatcher: SymbolMatcher = classSymbols.map(s => SymbolMatcher.exact(s.toString)).reduce(_ + _)

      val matchingTrees: Seq[Tree] = doc.tree.collect { case t: Tree if symbolMatcher.matches(t) => t }

      val fileName: Path = doc.input.asInstanceOf[Input.VirtualFile].path.pipe(Paths.get(_)).getFileName

      println()
      println(s"Usage of classes/traits/objects (one of ${classSymbols.mkString(", ")}) in file $fileName:")

      if (matchingTrees.isEmpty) {
        println("No usage found")
      } else {
        println()
        println(
          s"Kinds (classes) of matching trees: ${matchingTrees.map(_.getClass.getSimpleName).distinct.mkString(", ")}"
        )

        matchingTrees.foreach { tree =>
          println()
          println(s"Source file: $fileName")
          println(
            s"Position: line ${tree.pos.startLine}, col ${tree.pos.startColumn} .. line ${tree.pos.endLine}, col ${tree.pos.endColumn}"
          )
          printTreeInfo(tree, "")

          tree.parent.foreach { parentTree =>
            printTreeInfo(parentTree, "Parent tree -> ")

            parentTree.parent.foreach { grandParentTree =>
              printTreeInfo(grandParentTree, "Grandparent tree -> ")
            }
          }
        }
      }

      Patch.empty
    }
  }

  private def printTreeInfo(tree: Tree, messagePrefix: String)(implicit doc: SemanticDocument): Unit = {
    println(messagePrefix + s"Kind of tree (class name): ${tree.getClass.getSimpleName}")
    println(messagePrefix + s"Syntax: ${printSyntax(tree)}")
    println(messagePrefix + s"Symbol: ${tree.symbol}")
    println(messagePrefix + s"Symbol display name: ${tree.symbol.displayName}")
    println(messagePrefix + s"Symbol owner: ${tree.symbol.owner}")
  }

  private def checkClassSymbol(sym: Symbol)(implicit doc: SemanticDocument): Unit = {
    require(sym.info.forall(s => s.isClass || s.isTrait || s.isObject), s"Not a class/trait/object: $sym")
    require(sym.info.forall(_.isPublic), s"Not a public class/trait/object: $sym")
  }

  private def printSyntax(t: Tree): String = {
    val maxLength = 100

    val syntax = t.syntax

    if (syntax.length > maxLength) syntax.take(maxLength) + " ..." else syntax
  }

}

final case class UsedClassConfig(classSymbols: List[String] = Nil) {
  def isEmpty: Boolean = classSymbols.isEmpty
}

object UsedClassConfig {
  val default: UsedClassConfig = UsedClassConfig()
  implicit val surface: Surface[UsedClassConfig] = metaconfig.generic.deriveSurface[UsedClassConfig]
  implicit val decoder: ConfDecoder[UsedClassConfig] = metaconfig.generic.deriveDecoder(default)
}
