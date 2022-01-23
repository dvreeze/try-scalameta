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
 * The query API is intentionally far less minimal and less powerful than the Scalameta built-in method "collect",
 * but it gives a more XPath-like development experience to those using it. In that sense it may make many custom
 * traversals obsolete (where method "collect" does not cut it), while offering clear semantics and preventing
 * somewhat involved output collection creation (of custom traversals).
 *
 * This query API is somewhat overlapping with contrib.TreeOps.
 *
 * @author
 *   Chris de Vreeze
 */
object QuerySupport {

  trait QueryApi {

    // Child axis

    def filterChildren[A <: Tree : ClassTag](p: A => Boolean): List[A]

    def findFirstChild[A <: Tree : ClassTag](p: A => Boolean): Option[A]

    def findFirstChild[A <: Tree : ClassTag](): Option[A]

    // Descendant-or-self axis

    def filterDescendantsOrSelf[A <: Tree : ClassTag](p: A => Boolean): List[A]

    def findAllDescendantsOrSelf[A <: Tree : ClassTag](): List[A]

    def findFirstDescendantOrSelf[A <: Tree : ClassTag](p: A => Boolean): Option[A]

    def findFirstDescendantOrSelf[A <: Tree : ClassTag](): Option[A]

    // Descendant axis

    def filterDescendants[A <: Tree : ClassTag](p: A => Boolean): List[A]

    /**
     * Equivalent to `TreeOps.descendants(this).collect { case t: A => t }`
     */
    def findAllDescendants[A <: Tree : ClassTag](): List[A]

    def findFirstDescendant[A <: Tree : ClassTag](p: A => Boolean): Option[A]

    def findFirstDescendant[A <: Tree : ClassTag](): Option[A]

    // Like descendant-or-self axis, but only topmost

    def findTopmostOrSelf[A <: Tree : ClassTag](p: A => Boolean): List[A]

    def findAllTopmostOrSelf[A <: Tree : ClassTag](): List[A]

    // Like descendant axis, but only topmost

    def findTopmost[A <: Tree : ClassTag](p: A => Boolean): List[A]

    def findAllTopmost[A <: Tree : ClassTag](): List[A]

    // Ancestor-or-self axis

    def filterAncestorsOrSelf[A <: Tree : ClassTag](p: A => Boolean): List[A]

    def findAllAncestorsOrSelf[A <: Tree : ClassTag](): List[A]

    def findFirstAncestorOrSelf[A <: Tree : ClassTag](p: A => Boolean): Option[A]

    def findFirstAncestorOrSelf[A <: Tree : ClassTag](): Option[A]

    // Ancestor axis

    def filterAncestors[A <: Tree : ClassTag](p: A => Boolean): List[A]

    /**
     * Equivalent to `TreeOps.ancestors(this).collect { case t: A => t }`
     */
    def findAllAncestors[A <: Tree : ClassTag](): List[A]

    def findFirstAncestor[A <: Tree : ClassTag](p: A => Boolean): Option[A]

    def findFirstAncestor[A <: Tree : ClassTag](): Option[A]
  }

  implicit class WithQueryMethods(val tree: Tree) extends QueryApi {

    // Child axis

    def filterChildren[A <: Tree : ClassTag](p: A => Boolean): List[A] = {
      tree.children.collect { case t: A if p(t) => t }
    }

    def findFirstChild[A <: Tree : ClassTag](p: A => Boolean): Option[A] = filterChildren[A](p).headOption

    def findFirstChild[A <: Tree : ClassTag](): Option[A] = filterChildren[A](_ => true).headOption

    // Descendant-or-self axis

    def filterDescendantsOrSelf[A <: Tree : ClassTag](p: A => Boolean): List[A] = {
      val optSelf: List[A] = List(tree).collect { case t: A if p(t) => t }
      // Recursive
      tree.children.flatMap(_.filterDescendantsOrSelf[A](p)).prependedAll(optSelf)
    }

    def findAllDescendantsOrSelf[A <: Tree : ClassTag](): List[A] = filterDescendantsOrSelf[A](_ => true)

    def findFirstDescendantOrSelf[A <: Tree : ClassTag](p: A => Boolean): Option[A] = filterDescendantsOrSelf[A](p).headOption

    def findFirstDescendantOrSelf[A <: Tree : ClassTag](): Option[A] = findAllDescendantsOrSelf[A]().headOption

    // Descendant axis

    def filterDescendants[A <: Tree : ClassTag](p: A => Boolean): List[A] = {
      tree.children.flatMap(_.filterDescendantsOrSelf[A](p))
    }

    def findAllDescendants[A <: Tree : ClassTag](): List[A] = filterDescendants[A](_ => true)

    def findFirstDescendant[A <: Tree : ClassTag](p: A => Boolean): Option[A] = filterDescendants[A](p).headOption

    def findFirstDescendant[A <: Tree : ClassTag](): Option[A] = findAllDescendants[A]().headOption

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

    def findFirstAncestorOrSelf[A <: Tree : ClassTag](p: A => Boolean): Option[A] = filterAncestorsOrSelf[A](p).headOption

    def findFirstAncestorOrSelf[A <: Tree : ClassTag](): Option[A] = findAllAncestorsOrSelf[A]().headOption

    // Ancestor axis

    def filterAncestors[A <: Tree : ClassTag](p: A => Boolean): List[A] = {
      tree.parent.toList.flatMap(_.filterAncestorsOrSelf[A](p))
    }

    def findAllAncestors[A <: Tree : ClassTag](): List[A] = filterAncestors[A](_ => true)

    def findFirstAncestor[A <: Tree : ClassTag](p: A => Boolean): Option[A] = filterAncestors[A](p).headOption

    def findFirstAncestor[A <: Tree : ClassTag](): Option[A] = findAllAncestors[A]().headOption
  }
}
