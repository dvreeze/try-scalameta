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

/**
 * Shows method definitions and where they are used.
 *
 * This "rule" only depends on the Scala standard library and on Scalafix (and therefore Scalameta) and nothing else, so
 * this rule can easily be run from its source path. For example: "scalafix --rules=file:/path/to/ShowMethodUsage.scala"
 *
 * @author
 *   Chris de Vreeze
 */
class ShowMethodUsage extends SemanticRule("ShowMethodUsage") {

  override def fix(implicit doc: SemanticDocument): Patch = {
    val publicMethodDefnSymbols: Seq[Symbol] = doc.tree
      .collect { case t: Defn.Def =>
        t.symbol.info.ensuring(_.forall(_.isMethod)).ensuring(_.forall(_.isDef))
      }
      .collect { case Some(info) if info.isPublic => info.symbol }

    val publicMethodCalls: Seq[Term.Apply] = doc.tree.collect { case t: Term.Apply => t }
    val publicMethodCallsBySymbol: Map[Symbol, Seq[Term.Apply]] = publicMethodCalls.groupBy(_.fun.symbol)

    val fileName: Path = doc.input.asInstanceOf[Input.VirtualFile].path.pipe(Paths.get(_)).getFileName

    println()
    println(s"Method definitions (public) and their usage in $fileName:")

    publicMethodDefnSymbols.foreach { defSymbol =>
      println()
      println(s"Symbol: $defSymbol")
      println(s"\tDisplay name: ${defSymbol.displayName}")
      println(s"\tOwner: ${defSymbol.owner}")

      val methodCalls: Seq[Term.Apply] = publicMethodCallsBySymbol.getOrElse(defSymbol, Seq.empty)

      methodCalls.foreach { methodCall =>
        println(s"Method call (in ${methodCall.symbol.owner}):")
        println(s"\t${methodCall.syntax}")
      }
    }

    // TODO Turn this program into something useful

    Patch.empty
  }

}
