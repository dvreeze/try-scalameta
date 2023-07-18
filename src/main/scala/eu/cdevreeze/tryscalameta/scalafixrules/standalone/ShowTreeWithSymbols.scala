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

    val trees: Seq[Tree] = doc.tree.collect { case node: Tree => node }

    trees.foreach { tree =>
      val symbol: Symbol = tree.symbol
      val pos: String = s"[${tree.pos.startLine},${tree.pos.startColumn} .. ${tree.pos.endLine},${tree.pos.endColumn}]"
      println(s"Tree ${tree.getClass.getSimpleName} at position $pos has symbol: $symbol")
    }

    Patch.empty
  }

}
