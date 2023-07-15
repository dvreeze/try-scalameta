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

import scalafix.v1._

/**
 * Shows trees and their corresponding symbols.
 *
 * This "rule" only depends on the Scala standard library and on Scalafix (and therefore Scalameta) and nothing else, so
 * this rule can easily be run from its source path against sbt or Maven projects.
 *
 * @author
 *   Chris de Vreeze
 */
final class ShowSymbols extends SemanticRule("ShowSymbols") {

  override def fix(implicit doc: SemanticDocument): Patch = {
    val trees: Seq[Tree] = doc.tree.collect { case t: Tree => t } // Returns doc.tree as well, as first result

    val fileName: Path = doc.input.asInstanceOf[Input.VirtualFile].path.pipe(Paths.get(_)).getFileName

    println()
    println(s"Trees and their corresponding symbols in file $fileName:")

    trees.foreach(t => printTreeInfo(t, fileName))

    Patch.empty
  }

  private def printTreeInfo(t: Tree, fileName: Path)(implicit doc: SemanticDocument): Unit = {
    println()
    println(s"Source file: $fileName")
    println(
      s"Position: line ${t.pos.startLine}, col ${t.pos.startColumn} .. line ${t.pos.endLine}, col ${t.pos.endColumn}"
    )
    println(s"Kind of tree (class name): ${t.getClass.getSimpleName}")
    println(s"Syntax: ${printSyntax(t)}")
    println(s"Symbol: ${t.symbol}")
    println(s"Symbol owner: ${t.symbol.owner}")

    t match {
      case t: Stat =>
        println(s"isTopLevelStat: ${t.isTopLevelStat}")
        println(s"isTemplateStat: ${t.isTemplateStat}")
        println(s"isBlockStat: ${t.isBlockStat}")
        println(s"isEarlyStat: ${t.isEarlyStat}")
        println(s"isExistentialStat: ${t.isExistentialStat}")
        println(s"isRefineStat: ${t.isRefineStat}")
      case _ => ()
    }
  }

  private def printSyntax(t: Tree): String = {
    val maxLength = 100

    val syntax = t.syntax

    if (syntax.length > maxLength) syntax.take(maxLength) + " ..." else syntax
  }

}
