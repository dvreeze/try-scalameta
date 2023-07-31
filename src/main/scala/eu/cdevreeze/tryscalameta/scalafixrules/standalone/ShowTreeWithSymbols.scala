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

import scala.meta.Tree
import scala.meta.inputs.Input
import scala.reflect.ClassTag
import scala.util.chaining.scalaUtilChainingOps

import scalafix.XtensionScalafixProductInspect
import scalafix.patch.Patch
import scalafix.v1._

/**
 * Shows the tree structure, along with symbols.
 *
 * This "rule" only depends on the Scala standard library and on Scalafix (and therefore Scalameta) and nothing else, so
 * this rule can easily be run from its source path against sbt or Maven projects.
 *
 * @author
 *   Chris de Vreeze
 */
final class ShowTreeWithSymbols extends SemanticRule("ShowTree") {

  override def fix(implicit doc: SemanticDocument): Patch = {
    val fileName: Path = doc.input.asInstanceOf[Input.VirtualFile].path.pipe(Paths.get(_)).getFileName
    println()
    println(s"File: $fileName")

    println()
    println(doc.tree.structureLabeled(80))

    println()
    println("Symbols:")
    println()

    val trees: Seq[Tree] = findAllDescendantsOrSelf[Tree](doc.tree)

    trees.foreach { tree =>
      val symbol: Symbol = tree.symbol
      val pos: String = s"[${tree.pos.startLine},${tree.pos.startColumn} .. ${tree.pos.endLine},${tree.pos.endColumn}]"
      println(s"Tree ${tree.getClass.getSimpleName} at position $pos has symbol: $symbol")

      if (symbol.info.exists(_.signature.isInstanceOf[ClassSignature])) {
        val parentsOrSelf: List[Symbol] = getParentSymbolsOrSelf(symbol)
        parentsOrSelf.foreach { sym => println(s"\tSuper-type (or self): $sym") }
      }
    }

    Patch.empty
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

  private def findAllDescendantsOrSelf[A <: Tree: ClassTag](tree: Tree): List[A] =
    filterDescendantsOrSelf[A](tree, _ => true)

}
