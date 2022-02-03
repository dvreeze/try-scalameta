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

import scalafix.v1._
import scala.meta._
import scala.util.chaining.scalaUtilChainingOps

import metaconfig.ConfDecoder
import metaconfig.Configured
import metaconfig.generic.Surface

/**
 * Shows usage of public methods whose symbols are passed as configuration data.
 *
 * This "rule" only depends on the Scala standard library and on Scalafix (and therefore Scalameta) and nothing else, so
 * this rule can easily be run from its source path. For example: "scalafix --rules=file:/path/to/ShowMethodUsage.scala"
 *
 * @author
 *   Chris de Vreeze
 */
final class ShowMethodUsage(val config: UsedMethodConfig) extends SemanticRule("ShowMethodUsage") {

  def this() = this(UsedMethodConfig.default)

  private case class MatchingTerm(term: Term, methodSymbol: Symbol)

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

      val matchingTerms: Seq[MatchingTerm] = doc.tree.collect {
        case t: Term.Apply if methodSymbols.contains(t.fun.symbol) => MatchingTerm(t, t.fun.symbol)
        case t: Term.Name if methodSymbols.contains(t.symbol)      => MatchingTerm(t, t.symbol)
      }

      val fileName: Path = doc.input.asInstanceOf[Input.VirtualFile].path.pipe(Paths.get(_)).getFileName

      println()
      println(s"Usage of methods (one of ${methodSymbols.mkString(", ")}) in file $fileName:")

      if (matchingTerms.isEmpty) {
        println("No usage found")
      } else {
        matchingTerms.foreach { case MatchingTerm(term, methodSymbol) =>
          println()
          println(s"Usage of method $methodSymbol:")
          println(s"Term class name: ${term.getClass.getSimpleName}")
          println(s"Syntax: ${term.syntax}")
          println(s"Display name: ${term.symbol.displayName}")
          println(s"Owner: ${term.symbol.owner}")
        }
      }

      Patch.empty
    }
  }

  private def checkMethodSymbol(sym: Symbol)(implicit doc: SemanticDocument): Unit = {
    require(sym.info.forall(_.isMethod), s"Not a method: $sym")
    require(sym.info.forall(_.isPublic), s"Not a public method: $sym")
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
