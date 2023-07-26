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

package eu.cdevreeze.tryscalameta.scalafixrules.standalone.enterpriseuse

import java.nio.file.Path
import java.nio.file.Paths

import scala.meta.Defn
import scala.meta.Tree
import scala.meta.inputs.Input
import scala.reflect.ClassTag
import scala.util.chaining.scalaUtilChainingOps

import scalafix.v1._

/**
 * Shows "enterprise services", such as Servlet implementations, Thrift services, Kafka and RabbitMQ consumer and
 * producer types. This gives a quick overview of which of those services are offered by a code base.
 *
 * This "rule" only depends on the Scala standard library and on Scalafix (and therefore Scalameta) and nothing else, so
 * this rule can easily be run from its source path against sbt or Maven projects.
 *
 * @author
 *   Chris de Vreeze
 */
final class ShowEnterpriseServices extends SemanticRule("ShowEnterpriseServices") {

  private val servletTypeSymbolMatcher: SymbolMatcher = SymbolMatcher.normalized("javax.servlet.http.HttpServlet")

  override def fix(implicit doc: SemanticDocument): Patch = {
    val fileName: Path = doc.input.asInstanceOf[Input.VirtualFile].path.pipe(Paths.get(_)).getFileName

    showServletTypes(fileName)

    Patch.empty
  }

  private def showServletTypes(fileName: Path)(implicit doc: SemanticDocument): Unit = {
    val defns: Seq[Defn] = filterDescendantsOrSelf[Defn](
      doc.tree,
      t => getParentSymbolsOrSelf(t.symbol).exists(pt => servletTypeSymbolMatcher.matches(pt))
    )

    defns.foreach { defn =>
      println(s"In file '$fileName' servlet class '${defn.symbol}' found")
      println(s"\tSuper-types (or self): ${getParentSymbolsOrSelf(defn.symbol)}")
    }
  }

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

}
