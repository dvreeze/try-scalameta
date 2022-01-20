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

package eu.cdevreeze.tryscalameta.support

import scala.meta.Tree
import scala.reflect.ClassTag

/**
 * Tree query support reminding somewhat of XPath axes. This API is not geared towards pattern matching, though.
 *
 * @author
 *   Chris de Vreeze
 */
object QuerySupport {

  implicit class WithQueryMethods(val tree: Tree) extends AnyVal {

    // Child axis

    def filterChildren[A <: Tree : ClassTag](p: A => Boolean): List[A] = {
      tree.children.collect { case t: A if p(t) => t }
    }

    // Descendant-or-self axis

    def filterDescendantsOrSelf[A <: Tree : ClassTag](p: A => Boolean): List[A] = {
      val optSelf: List[A] = List(tree).collect { case t: A if p(t) => t }
      // Recursive
      tree.children.flatMap(_.filterDescendantsOrSelf[A](p)).prependedAll(optSelf)
    }

    def findAllDescendantsOrSelf[A <: Tree : ClassTag](): List[A] = filterDescendantsOrSelf[A](_ => true)

    // Descendant axis

    def filterDescendants[A <: Tree : ClassTag](p: A => Boolean): List[A] = {
      tree.children.flatMap(_.filterDescendantsOrSelf[A](p)) // Same as collect, it seems
    }

    def findAllDescendants[A <: Tree : ClassTag](): List[A] = filterDescendants[A](_ => true)

    // Like descendant-or-self axis, but only topmost

    def findTopmostOrSelf[A <: Tree : ClassTag](p: A => Boolean): List[A] = {
      val optSelf: List[A] = List(tree).collect { case t: A if p(t) => t }

      if (optSelf.nonEmpty) {
        optSelf
      } else {
        // Recursive
        tree.children.flatMap(_.findTopmostOrSelf[A](p))
      }
    }

    def findAllTopmostOrSelf[A <: Tree : ClassTag](): List[A] = findTopmostOrSelf[A](_ => true)

    // Like descendant axis, but only topmost

    def findTopmost[A <: Tree : ClassTag](p: A => Boolean): List[A] = {
      tree.children.flatMap(_.findTopmostOrSelf[A](p))
    }

    def findAllTopmost[A <: Tree : ClassTag](): List[A] = findTopmost[A](_ => true)

    // Ancestor-or-self axis

    def filterAncestorsOrSelf[A <: Tree : ClassTag](p: A => Boolean): List[A] = {
      val optSelf: List[A] = List(tree).collect { case t: A if p(t) => t }
      // Recursive
      tree.parent.toList.flatMap(_.filterAncestorsOrSelf[A](p)).prependedAll(optSelf)
    }

    def findAllAncestorsOrSelf[A <: Tree : ClassTag](): List[A] = filterAncestorsOrSelf[A](_ => true)

    // Ancestor axis

    def filterAncestors[A <: Tree : ClassTag](p: A => Boolean): List[A] = {
      tree.parent.toList.flatMap(_.filterAncestorsOrSelf[A](p))
    }

    def findAllAncestors[A <: Tree : ClassTag](): List[A] = filterAncestors[A](_ => true)

    def findAncestor[A <: Tree : ClassTag](p: A => Boolean): Option[A] = filterAncestors[A](p).headOption

    def findAncestor[A <: Tree : ClassTag](): Option[A] = findAllAncestors[A]().headOption
  }
}
