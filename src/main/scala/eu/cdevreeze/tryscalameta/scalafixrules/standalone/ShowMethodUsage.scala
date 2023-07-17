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

package eu.cdevreeze.tryscalameta.scalafixrules.standalone

import java.nio.file.Path
import java.nio.file.Paths

import scala.meta._
import scala.util.chaining.scalaUtilChainingOps

import metaconfig.ConfDecoder
import metaconfig.Configured
import metaconfig.generic.Surface
import scalafix.v1._

/**
 * Shows usage of public methods whose symbols are passed as configuration data. Only exact matches are returned,
 * without following any inheritance chains.
 *
 * This "rule" only depends on the Scala standard library and on Scalafix (and therefore Scalameta) and nothing else, so
 * this rule can easily be run from its source path against sbt or Maven projects.
 *
 * @author
 *   Chris de Vreeze
 */
final class ShowMethodUsage(val config: UsedMethodConfig) extends SemanticRule("ShowMethodUsage") {

  def this() = this(UsedMethodConfig.default)

  override def withConfiguration(config: Configuration): Configured[Rule] =
    config.conf
      .getOrElse("ShowMethodUsage")(this.config)
      .map(newConfig => new ShowMethodUsage(newConfig))

  override def fix(implicit doc: SemanticDocument): Patch = {
    if (config.isEmpty) {
      println("Missing methodSymbols (symbols of methods). Doing nothing.")
      Patch.empty
    } else {
      // Replacing ".." in HOCON by "#"
      val methodSymbols: Seq[Symbol] =
        config.methodSymbols.ensuring(_.nonEmpty).map(_.replace("..", "#")).map(Symbol.apply)

      methodSymbols.foreach(checkMethodSymbol)

      val symbolMatcher: SymbolMatcher = methodSymbols.map(s => SymbolMatcher.exact(s.toString)).reduce(_ + _)

      val matchingTrees: Seq[Tree] = doc.tree.collect { case t: Tree if symbolMatcher.matches(t) => t }

      val fileName: Path = doc.input.asInstanceOf[Input.VirtualFile].path.pipe(Paths.get(_)).getFileName

      println()
      println(s"Usage of methods (one of ${methodSymbols.mkString(", ")}) in file $fileName:")

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

  private def checkMethodSymbol(sym: Symbol)(implicit doc: SemanticDocument): Unit = {
    require(sym.info.forall(_.isMethod), s"Not a method: $sym")
    require(sym.info.forall(_.isPublic), s"Not a public method: $sym")
  }

  private def printSyntax(t: Tree): String = {
    val maxLength = 100

    val syntax = t.syntax

    if (syntax.length > maxLength) syntax.take(maxLength) + " ..." else syntax
  }

}

final case class UsedMethodConfig(methodSymbols: List[String] = Nil) {
  def isEmpty: Boolean = methodSymbols.isEmpty
}

object UsedMethodConfig {
  val default: UsedMethodConfig = UsedMethodConfig()
  implicit val surface: Surface[UsedMethodConfig] = metaconfig.generic.deriveSurface[UsedMethodConfig]
  implicit val decoder: ConfDecoder[UsedMethodConfig] = metaconfig.generic.deriveDecoder(default)
}
