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

package eu.cdevreeze.tryscalameta.scalafixrules.scalause

import scala.meta._
import scala.reflect.ClassTag

import scalafix.Patch
import scalafix.lint.Diagnostic
import scalafix.lint.LintSeverity
import scalafix.v1.SyntacticDocument
import scalafix.v1.SyntacticRule

/**
 * Checks nested case classes for not being nested inside traits or classes. It is rather strange for case classes to
 * have state depending on state of an encapsulating instance of a trait or class, after all.
 *
 * This "rule" only depends on the Scala standard library and on Scalafix (and therefore Scalameta) and nothing else, so
 * this rule can easily be run from its source path against sbt or Maven projects. For example: "scalafix
 * --rules=file:/path/to/NestedCaseClasses.scala --config=/path/to/scalafix-config-file --classpath=./target/classes"
 *
 * The classpath configuration option should point to the parent directory of a "META-INF/semanticdb/src/main/scala"
 * directory, where the "*.scala.semanticdb" files live.
 *
 * With "-f" or "--files" (once or more) specific (relative) Scala source directories can be chosen.
 *
 * See https://scalacenter.github.io/scalafix/docs/users/installation.html#command-line for more information.
 *
 * @author
 *   Chris de Vreeze
 */
final class NestedCaseClasses extends SyntacticRule("NestedCaseClasses") {

  override def fix(implicit doc: SyntacticDocument): Patch = {
    val caseClassDefns: Seq[Defn.Class] = doc.tree.collect { case t: Defn.Class if isCaseClassDefn(t) => t }

    val patches: Seq[Patch] = caseClassDefns
      .map { caseClassDefn =>
        val wronglyNested = filterAncestors(caseClassDefn, isClassOrTraitDefn).nonEmpty

        if (wronglyNested) {
          Patch.lint(
            Diagnostic(
              id = "case class",
              message = "case class nested inside another class or trait",
              position = caseClassDefn.pos
            )
          )
        } else {
          Patch.lint(
            Diagnostic(
              id = "case class",
              message = "case class encountered",
              position = caseClassDefn.pos,
              severity = LintSeverity.Info
            )
          )
        }
      }

    patches.asPatch
  }

  private def isCaseClassDefn(tree: Tree): Boolean = {
    tree match {
      case classDefn: Defn.Class if classDefn.mods.exists(isCase) => true
      case _                                                      => false
    }
  }

  // See https://github.com/scalameta/scalameta/issues/467
  private def isCase(mod: Mod): Boolean = mod match {
    case mod"case" => true
    case _         => false
  }

  private def isClassOrTraitDefn(tree: Tree): Boolean = {
    tree match {
      case _: Defn.Class => true
      case _: Defn.Trait => true
      case _             => false
    }
  }

  private def filterAncestorsOrSelf[A <: Tree: ClassTag](tree: Tree, p: A => Boolean): List[A] = {
    val optSelf: List[A] = List(tree).collect { case t: A if p(t) => t }
    // Recursive
    tree.parent.toList.flatMap(pt => filterAncestorsOrSelf[A](pt, p)).prependedAll(optSelf)
  }

  private def filterAncestors[A <: Tree: ClassTag](tree: Tree, p: A => Boolean): List[A] = {
    tree.parent.toList.flatMap(pt => filterAncestorsOrSelf[A](pt, p))
  }

}
