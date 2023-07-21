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

import scala.meta._
import scala.reflect.ClassTag

import scalafix.v1._

/**
 * Strips implementations of topmost def/val/var definitions. The goal is to get a quick overview of how a certain code
 * base hangs together without being bogged by implementation details. For that, make a copy of that code base and run
 * this rule on that copy. Ideally, the result of this rule still compiles successfully, in order to look at that
 * stripped code base in a Scala IDE. So, to be defensive, definitions are not patched if their types must be inferred
 * instead of them being made explicit.
 *
 * Another subsequent rule could then remove those private members that are not needed for successful compilation. Also,
 * a rule must then run to organise imports etc.
 *
 * This "rule" only depends on the Scala standard library and on Scalafix (and therefore Scalameta) and nothing else, so
 * this rule can easily be run from its source path against sbt or Maven projects.
 *
 * @author
 *   Chris de Vreeze
 */
final class StripImplementations extends SemanticRule("StripImplementation") {

  override def fix(implicit doc: SemanticDocument): Patch = {
    val topLevelDefns: Seq[Defn] = findAllTopmost[Defn](doc.tree)

    val patches: Seq[Patch] = topLevelDefns.map {
      case defn: Defn.Class  => processClassDefn(defn)
      case defn: Defn.Trait  => processTraitDefn(defn)
      case defn: Defn.Object => processObjectDefn(defn)
      case defn: Defn.Enum   => processEnumDefn(defn)
      case defn: Defn.Val    => processValDefn(defn)
      case defn: Defn.Var    => processVarDefn(defn)
      case defn: Defn.Def    => processDefDefn(defn)
      case defn: Defn.Type   => processTypeDefn(defn)
      case defn: Defn.Given  => processGivenDefn(defn)
      case _                 => Patch.empty
    }
    patches.asPatch
  }

  private def processClassDefn(defn: Defn.Class)(implicit doc: SemanticDocument): Patch = {
    processTemplate(defn.templ)
  }

  private def processTraitDefn(defn: Defn.Trait)(implicit doc: SemanticDocument): Patch = {
    processTemplate(defn.templ)
  }

  private def processObjectDefn(defn: Defn.Object)(implicit doc: SemanticDocument): Patch = {
    processTemplate(defn.templ)
  }

  private def processEnumDefn(defn: Defn.Enum)(implicit doc: SemanticDocument): Patch = {
    processTemplate(defn.templ)
  }

  private def processValDefn(defn: Defn.Val)(implicit doc: SemanticDocument): Patch = {
    if (defn.decltpe.nonEmpty) Patch.replaceTree(defn.body, "???") else Patch.empty
  }

  private def processVarDefn(defn: Defn.Var)(implicit doc: SemanticDocument): Patch = {
    if (defn.decltpe.nonEmpty) Patch.replaceTree(defn.body, "???") else Patch.empty
  }

  private def processDefDefn(defn: Defn.Def)(implicit doc: SemanticDocument): Patch = {
    if (defn.decltpe.nonEmpty) {
      val bodyPatch = Patch.replaceTree(defn.body, "???")

      val paramPatches: Seq[Patch] = defn.paramClauseGroups.flatMap { paramClauseGroup =>
        paramClauseGroup.paramClauses.flatMap { paramClause =>
          paramClause.values.collect {
            case param if param.decltpe.nonEmpty && param.default.nonEmpty =>
              Patch.replaceTree(param.default.get, "???")
          }
        }
      }

      paramPatches.asPatch + bodyPatch
    } else {
      Patch.empty
    }
  }

  private def processTypeDefn(defn: Defn.Type)(implicit doc: SemanticDocument): Patch = {
    Patch.empty
  }

  private def processGivenDefn(defn: Defn.Given)(implicit doc: SemanticDocument): Patch = {
    processTemplate(defn.templ)
  }

  private def processTemplate(template: Template)(implicit document: SemanticDocument): Patch = {
    // Recursion
    val patches: Seq[Patch] = template.stats.map {
      case defn: Defn.Class  => processClassDefn(defn)
      case defn: Defn.Trait  => processTraitDefn(defn)
      case defn: Defn.Object => processObjectDefn(defn)
      case defn: Defn.Enum   => processEnumDefn(defn)
      case defn: Defn.Val    => processValDefn(defn)
      case defn: Defn.Var    => processVarDefn(defn)
      case defn: Defn.Def    => processDefDefn(defn)
      case defn: Defn.Type   => processTypeDefn(defn)
      case defn: Defn.Given  => processGivenDefn(defn)
      case _                 => Patch.empty
    }
    patches.asPatch
  }

  // Tree navigation support

  private def findTopmostOrSelf[A <: Tree: ClassTag](tree: Tree, p: A => Boolean): List[A] = {
    val optSelf: List[A] = List(tree).collect { case t: A if p(t) => t }

    if (optSelf.nonEmpty) {
      optSelf
    } else {
      // Recursive
      tree.children.flatMap(ch => findTopmostOrSelf[A](ch, p))
    }
  }

  private def findTopmost[A <: Tree: ClassTag](tree: Tree, p: A => Boolean): List[A] =
    tree.children.flatMap(ch => findTopmostOrSelf[A](ch, p))

  private def findAllTopmost[A <: Tree: ClassTag](tree: Tree): List[A] = findTopmost[A](tree, _ => true)
}
