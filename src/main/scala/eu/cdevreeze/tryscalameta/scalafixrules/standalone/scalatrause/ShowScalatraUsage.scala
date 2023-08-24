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

package eu.cdevreeze.tryscalameta.scalafixrules.standalone.scalatrause

import java.nio.file.Path
import java.nio.file.Paths

import scala.meta._
import scala.reflect.ClassTag
import scala.util.chaining.scalaUtilChainingOps

import scalafix.v1._

/**
 * Shows usage of Scalatra, in particular servlet implementations, LifeCycle implementations and mount method calls.
 *
 * This "rule" only depends on the Scala standard library and on Scalafix (and therefore Scalameta) and nothing else, so
 * this rule can easily be run from its source path against sbt or Maven projects.
 *
 * @author
 *   Chris de Vreeze
 */
final class ShowScalatraUsage extends SemanticRule("ShowScalatraUsage") {

  override def fix(implicit doc: SemanticDocument): Patch = {
    val fileName: Path = doc.input.asInstanceOf[Input.VirtualFile].path.pipe(Paths.get(_)).getFileName

    println()
    println(s"Scalatra usage in file $fileName:")

    val publicServletClasses: Seq[Defn] = findPublicServletClasses(doc)

    if (publicServletClasses.nonEmpty) {
      println()
      publicServletClasses.foreach { cls =>
        println(s"Found public servlet class '${cls.symbol}' (is Scalatra Handler: ${isScalatraServlet(cls)})")
      }
    }

    val lifecycleClasses: Seq[Defn] = findLifeCycleClasses(doc)

    if (lifecycleClasses.nonEmpty) {
      println()
      lifecycleClasses.foreach { cls =>
        println(s"Found Scalatra LifeCycle implementation '${cls.symbol}'")
      }
    }

    val mountCallSymbols: Seq[Symbol] = findMountCalls(doc).map(_.symbol).distinct

    if (mountCallSymbols.nonEmpty) {
      println()
      println(s"Found at least one RichServletContext.mount call")
    }

    Patch.empty
  }

  private def findPublicServletClasses(implicit doc: SemanticDocument): Seq[Defn] = {
    val httpServletSymbolMatcher: SymbolMatcher = SymbolMatcher.exact("javax/servlet/http/HttpServlet#")

    def isMatch(defn: Defn): Boolean = {
      defn.symbol.info.exists(_.isPublic) &&
      getParentSymbolsOrSelf(defn.symbol).exists(httpServletSymbolMatcher.matches)
    }

    filterDescendants[Defn.Class](doc.tree, isMatch)
      .appendedAll(filterDescendants[Defn.Object](doc.tree, isMatch))
  }

  private def isScalatraServlet(servletClass: Defn)(implicit doc: SemanticDocument): Boolean = {
    val scalatraHandlerMatcher: SymbolMatcher = SymbolMatcher.exact("org/scalatra/Handler#")

    getParentSymbolsOrSelf(servletClass.symbol).exists(scalatraHandlerMatcher.matches)
  }

  private def findLifeCycleClasses(implicit doc: SemanticDocument): Seq[Defn] = {
    val lifecycleMatcher: SymbolMatcher = SymbolMatcher.exact("org/scalatra/LifeCycle#")

    def isMatch(defn: Defn): Boolean = {
      getParentSymbolsOrSelf(defn.symbol).exists(lifecycleMatcher.matches)
    }

    filterDescendants[Defn.Class](doc.tree, isMatch)
      .appendedAll(filterDescendants[Defn.Object](doc.tree, isMatch))
  }

  private def findMountCalls(implicit doc: SemanticDocument): Seq[Term] = {
    val mountMatcher: SymbolMatcher = SymbolMatcher.normalized("org.scalatra.servlet.RichServletContext.mount")

    filterDescendants[Term](doc.tree, mountMatcher.matches)
  }

  // Helpers

  private def getParentSymbolsOrSelf(symbol: Symbol)(implicit doc: SemanticDocument): List[Symbol] = {
    symbol.info match {
      case None => List(symbol)
      case Some(symbolInfo) =>
        symbolInfo.signature match {
          case ClassSignature(_, parents, _, _) =>
            List(symbol).appendedAll {
              parents
                .collect { case TypeRef(_, parentSymbol, _) =>
                  // Recursive call
                  getParentSymbolsOrSelf(parentSymbol)(doc)
                }
                .flatten
                .distinct
            }
          case _ => List(symbol)
        }
    }
  }

  // Tree navigation support

  private def filterDescendantsOrSelf[A <: Tree: ClassTag](tree: Tree, p: A => Boolean): List[A] = {
    val optSelf: List[A] = List(tree).collect { case t: A if p(t) => t }
    // Recursive
    tree.children.flatMap(ch => filterDescendantsOrSelf[A](ch, p)).prependedAll(optSelf)
  }

  private def filterDescendants[A <: Tree: ClassTag](tree: Tree, p: A => Boolean): List[A] = {
    tree.children.flatMap(ch => filterDescendantsOrSelf[A](ch, p))
  }

}
