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

package eu.cdevreeze.tryscalameta.scalafixrules.standalone.seleniumuse

import java.nio.file.Path
import java.nio.file.Paths

import scala.meta.Defn
import scala.meta.Term
import scala.meta.Tree
import scala.meta.inputs.Input
import scala.reflect.ClassTag
import scala.util.chaining.scalaUtilChainingOps

import scalafix.v1._

/**
 * Shows use of Selenium XPath Locators. These XPath locators are very brittle in matching on CSS classes (separated by
 * spaces), and therefore it would be nice to get an overview of where they are used (instead of CSS selectors, for
 * example).
 *
 * This "rule" only depends on the Scala standard library and on Scalafix (and therefore Scalameta) and nothing else, so
 * this rule can easily be run from its source path against sbt or Maven projects.
 *
 * @author
 *   Chris de Vreeze
 */
final class ShowUseOfXPathLocators extends SemanticRule("ShowUseOfXPathLocators") {

  private val byXPathSymbolMatcher: SymbolMatcher = SymbolMatcher.normalized("org.openqa.selenium.By.xpath")

  override def fix(implicit doc: SemanticDocument): Patch = {
    val usesOfByXPath: Seq[Term] = doc.tree.collect { case t: Term if isUseOfXPathLocator(t)(doc) => t }

    val fileName: Path = doc.input.asInstanceOf[Input.VirtualFile].path.pipe(Paths.get(_)).getFileName

    usesOfByXPath.foreach { useOfByXPath =>
      findSurroundingDefn(useOfByXPath)
        .map { defn =>
          println(s"Use of 'By.xpath' in file '$fileName', within definition ${defn.symbol}")
        }
        .getOrElse {
          println(s"Use of 'By.xpath' in file '$fileName'")
        }
    }

    Patch.empty
  }

  private def isUseOfXPathLocator(node: Tree)(implicit doc: SemanticDocument): Boolean = {
    node match {
      case term: Term if byXPathSymbolMatcher.matches(term) => true
      case _                                                => false
    }
  }

  private def findSurroundingDefn(node: Tree)(implicit doc: SemanticDocument): Option[Defn] = {
    findFirstAncestor[Defn](node, _ => true)
  }

  // Tree navigation support

  private def filterAncestorsOrSelf[A <: Tree: ClassTag](tree: Tree, p: A => Boolean): List[A] = {
    val optSelf: List[A] = List(tree).collect { case t: A if p(t) => t }
    // Recursive
    tree.parent.toList.flatMap(parent => filterAncestorsOrSelf[A](parent, p)).prependedAll(optSelf)
  }

  private def filterAncestors[A <: Tree: ClassTag](tree: Tree, p: A => Boolean): List[A] = {
    tree.parent.toList.flatMap(parent => filterAncestorsOrSelf[A](parent, p))
  }

  private def findFirstAncestor[A <: Tree: ClassTag](tree: Tree, p: A => Boolean): Option[A] =
    filterAncestors[A](tree, p).headOption

}
