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
 * this rule can easily be run from its source path. For example:
 * "scalafix --rules=file:/path/to/ShowMethodUsage.scala"
 *
 * @author
 *   Chris de Vreeze
 */
class ShowMethodUsage extends SemanticRule("ShowMethodUsage") {

  override def fix(implicit doc: SemanticDocument): Patch = {
    val publicMethodDefs: Seq[Symbol] = doc.tree
      .collect { case t: Defn.Def =>
        t.symbol.info
      }
      .collect {
        case Some(info) if info.isPublic =>
          info.ensuring(_.isDef).symbol
      }

    val publicMethodCalls: Seq[Symbol] = doc.tree.collect { case t: Term.Apply =>
      t.fun.symbol
    }

    val fileName: Path = doc.input.asInstanceOf[Input.VirtualFile].path.pipe(Paths.get(_)).getFileName

    println()
    println(s"Method definitions (public) in $fileName")
    println()
    publicMethodDefs.foreach(m => println(s"$m\n\tDisplay name: ${m.displayName}\n\tOwner: ${m.owner}"))

    println()
    println(s"Method calls in $fileName")
    println()
    publicMethodCalls.foreach(m => println(s"$m\n\tDisplay name: ${m.displayName}\n\tOwner: ${m.owner}"))

    // TODO Find usages of those method definitions
    // TODO Better: summarize method definitions and method calls per source file, and use another program to reason about them

    Patch.empty
  }

}
