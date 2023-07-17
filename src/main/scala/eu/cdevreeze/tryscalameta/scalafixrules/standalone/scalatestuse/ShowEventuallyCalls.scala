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

package eu.cdevreeze.tryscalameta.scalafixrules.standalone.scalatestuse

import java.nio.file.Path
import java.nio.file.Paths

import scala.meta.Term
import scala.meta.Tree
import scala.meta.inputs.Input
import scala.reflect.ClassTag
import scala.util.chaining.scalaUtilChainingOps

import scalafix.patch.Patch
import scalafix.v1._

/**
 * Shows calls to function(s) "eventually". This helps detect nested calls to function(s) "eventually".
 *
 * This "rule" only depends on the Scala standard library and on Scalafix (and therefore Scalameta) and nothing else, so
 * this rule can easily be run from its source path against sbt or Maven projects.
 *
 * @author
 *   Chris de Vreeze
 */
final class ShowEventuallyCalls extends SemanticRule("ShowEventuallyCalls") {

  override def fix(implicit doc: SemanticDocument): Patch = {
    val docContainsEventuallyCall =
      doc.tree.collect { case t: Term if isEventuallyFunction(t.symbol) => t }.nonEmpty

    val fileName: Path = doc.input.asInstanceOf[Input.VirtualFile].path.pipe(Paths.get(_)).getFileName

    if (docContainsEventuallyCall) {
      println()
      println(s"Investigating file $fileName, which contains an \"eventually\" call ...")

      val eventuallyCalls: Seq[Term.Apply] = {
        filterDescendants[Term.Apply](doc.tree, t => isCompleteFunctionCall(t) && isEventuallyFunction(t.symbol))
      }

      eventuallyCalls.foreach { eventuallyCall =>
        println(s"\"eventually\" call at ${eventuallyCall.pos.pipe(pos => s"[${pos.startLine}..${pos.endLine}]")}")

        val syntacticallyNestedEventuallyCalls =
          filterDescendants[Term.Apply](
            eventuallyCall,
            t => isCompleteFunctionCall(t) && isEventuallyFunction(t.symbol)
          )

        if (syntacticallyNestedEventuallyCalls.nonEmpty) {
          println(s"It has syntactically nested \"eventually\" calls!!")
        }

        val functionCalls: Seq[Term.Apply] =
          filterDescendants[Term.Apply](
            eventuallyCall,
            t => isCompleteFunctionCall(t) && isProbablyNonStdlibFunction(t.symbol)
          )

        functionCalls.foreach { functionCall =>
          println(
            s"\tNested function call to function: ${functionCall.symbol.displayName} (in ${functionCall.symbol.owner})"
          )
        }
      }
    }

    Patch.empty
  }

  private def isEventuallyFunction(symbol: Symbol)(implicit doc: SemanticDocument): Boolean = {
    // Not investigating potentially absent SymbolInformation
    symbol.displayName == "eventually" && symbol.owner.toString == "org/scalatest/concurrent/Eventually#"
  }

  private def isCompleteFunctionCall(t: Term.Apply)(implicit doc: SemanticDocument): Boolean = {
    isProbablyFunction(t.symbol) && {
      // Returns true if this is a function call with the complete argument lists, and not just its first "same function" call ancestor leaving out the last argument list
      filterAncestors[Term.Apply](
        t,
        { ancestor =>
          ancestor.symbol == t.symbol && ancestor.pos.startLine == t.pos.startLine && ancestor.pos.startColumn == t.pos.startColumn
        }
      ).isEmpty
    }
  }

  private def isProbablyNonStdlibFunction(symbol: Symbol): Boolean = {
    isProbablyFunction(symbol) && !symbol.toString.startsWith("scala/") && !symbol.toString.startsWith("java/")
  }

  private def isProbablyFunction(symbol: Symbol): Boolean = {
    // This circumvents potentially failing attempts to get the optional SymbolInformation (outside the current implicit SemanticDocument).
    symbol.toString.contains('(') && symbol.toString.contains(')')
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

  private def filterAncestorsOrSelf[A <: Tree: ClassTag](tree: Tree, p: A => Boolean): List[A] = {
    val optSelf: List[A] = List(tree).collect { case t: A if p(t) => t }
    // Recursive
    tree.parent.toList.flatMap(parent => filterAncestorsOrSelf[A](parent, p)).prependedAll(optSelf)
  }

  private def filterAncestors[A <: Tree: ClassTag](tree: Tree, p: A => Boolean): List[A] = {
    tree.parent.toList.flatMap(parent => filterAncestorsOrSelf[A](parent, p))
  }

}
