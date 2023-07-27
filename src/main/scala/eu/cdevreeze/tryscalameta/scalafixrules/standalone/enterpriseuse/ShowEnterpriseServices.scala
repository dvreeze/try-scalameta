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

package eu.cdevreeze.tryscalameta.scalafixrules.standalone.enterpriseuse

import java.nio.file.Path
import java.nio.file.Paths

import scala.meta._
import scala.meta.inputs.Input
import scala.reflect.ClassTag
import scala.util.chaining.scalaUtilChainingOps

import metaconfig.ConfDecoder
import metaconfig.generic.Surface
import scalafix.v1._

/**
 * Shows "enterprise services", such as Servlet implementations, Thrift services, Kafka and RabbitMQ consumer and
 * producer types. This gives a quick overview of which of those services are offered by a code base.
 *
 * This "rule" only depends on the Scala standard library and on Scalafix (and therefore Scalameta) and nothing else, so
 * this rule can easily be run from its source path against sbt or Maven projects.
 *
 * @author
 *   Chris de Vreeze
 */
final class ShowEnterpriseServices(val config: EnterpriseServiceConfig) extends SemanticRule("ShowEnterpriseServices") {

  private val servletTypeSymbolMatcher: SymbolMatcher = SymbolMatcher.exact("javax/servlet/http/HttpServlet#")

  def this() = this(EnterpriseServiceConfig.default)

  override def fix(implicit doc: SemanticDocument): Patch = {
    if (config.isEmpty) {
      println(
        "Missing serviceTypeSymbols (symbols of service types, such as Kafka consumer, Thrift services, etc.). Doing nothing."
      )
      Patch.empty
    } else {
      // Replacing ".." in HOCON by "#"
      val serviceTypeSymbols: Seq[Symbol] =
        config.serviceTypeSymbols.ensuring(_.nonEmpty).map(_.replace("..", "#")).map(Symbol.apply)

      serviceTypeSymbols.foreach(checkClassSymbol)

      val fileName: Path = doc.input.asInstanceOf[Input.VirtualFile].path.pipe(Paths.get(_)).getFileName

      showServletTypes(fileName)

      serviceTypeSymbols.foreach { serviceTpe =>
        val symbolMatcher = SymbolMatcher.exact(serviceTpe.toString)

        showServiceTypes(symbolMatcher, fileName)(doc)
      }

      Patch.empty
    }
  }

  private def showServiceTypes(serviceTypeSymbolMatcher: SymbolMatcher, fileName: Path)(implicit
      doc: SemanticDocument
  ): Unit = {
    val defns: Seq[Defn.Class] = filterDescendantsOrSelf[Defn.Class](
      doc.tree,
      t =>
        getParentSymbolsOrSelf(t.symbol).exists { pt =>
          serviceTypeSymbolMatcher.matches(pt)
        } && !t.mods.exists(isAbstract)
    )

    defns.foreach { defn =>
      println(s"In file '$fileName' '${defn.symbol.displayName}' service class '${defn.symbol}' found")

      getParentSymbolsOrSelf(defn.symbol).foreach { superTpe =>
        println(s"\tSuper-type (or self): $superTpe")
      }
    }
  }

  private def showServletTypes(fileName: Path)(implicit doc: SemanticDocument): Unit = {
    showServiceTypes(servletTypeSymbolMatcher, fileName)(doc)
  }

  // See https://github.com/scalameta/scalameta/issues/467
  private def isAbstract(mod: Mod): Boolean = mod match {
    case mod"abstract" => true
    case _             => false
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

  private def checkClassSymbol(sym: Symbol)(implicit doc: SemanticDocument): Unit = {
    require(
      sym.info.exists(info => info.isClass || info.isTrait || info.isInterface),
      s"Not a class/trait/interface: $sym"
    )
    require(sym.info.exists(_.isPublic), s"Not a public class/trait/interface: $sym")
  }

  // Tree navigation support

  private def filterDescendantsOrSelf[A <: Tree: ClassTag](tree: Tree, p: A => Boolean): List[A] = {
    val optSelf: List[A] = List(tree).collect { case t: A if p(t) => t }
    // Recursive
    tree.children.flatMap(ch => filterDescendantsOrSelf[A](ch, p)).prependedAll(optSelf)
  }

}

final case class EnterpriseServiceConfig(serviceTypeSymbols: List[String] = Nil) {
  def isEmpty: Boolean = serviceTypeSymbols.isEmpty
}

object EnterpriseServiceConfig {
  val default: EnterpriseServiceConfig = EnterpriseServiceConfig()
  implicit val surface: Surface[EnterpriseServiceConfig] = metaconfig.generic.deriveSurface[EnterpriseServiceConfig]
  implicit val decoder: ConfDecoder[EnterpriseServiceConfig] = metaconfig.generic.deriveDecoder(default)
}
