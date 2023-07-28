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

import eu.cdevreeze.tryscalameta.scalafixrules.standalone.enterpriseuse.ShowEnterpriseServices.ServiceMatcher
import eu.cdevreeze.tryscalameta.scalafixrules.standalone.enterpriseuse.ShowEnterpriseServices.ServiceMatcher.ServiceMatcherOnSuperType
import metaconfig.ConfDecoder
import metaconfig.Configured
import metaconfig.generic.Surface
import scalafix.v1._

/**
 * Shows "enterprise services", such as Servlet implementations, Thrift services, Kafka and RabbitMQ consumer and
 * producer types, depending on the configuration. This gives a quick overview of which of those services are offered by
 * a code base.
 *
 * This "rule" only depends on the Scala standard library and on Scalafix (and therefore Scalameta) and nothing else, so
 * this rule can easily be run from its source path against sbt or Maven projects.
 *
 * @author
 *   Chris de Vreeze
 */
final class ShowEnterpriseServices(val config: EnterpriseServiceConfig) extends SemanticRule("ShowEnterpriseServices") {

  def this() = this(EnterpriseServiceConfig.default)

  override def withConfiguration(config: Configuration): Configured[Rule] =
    config.conf
      .getOrElse("ShowEnterpriseServices")(this.config)
      .map(newConfig => new ShowEnterpriseServices(newConfig))

  override def fix(implicit doc: SemanticDocument): Patch = {
    if (config.isEmpty) {
      println(
        "Missing \"enterprise service matchers\". Doing nothing."
      )
      Patch.empty
    } else {
      val serviceMatchers: Seq[ServiceMatcher] = config.entries.map(entry => ServiceMatcher.fromConfigEntry(entry))

      val fileName: Path = doc.input.asInstanceOf[Input.VirtualFile].path.pipe(Paths.get(_)).getFileName

      val matchingServletDefns = collectMatchingDefinitions(ServiceMatcher.default)
      showServiceTypes(matchingServletDefns, fileName, ServiceMatcher.default.serviceNameinMessage)

      serviceMatchers.foreach { serviceMatcher =>
        val matchingDefns = collectMatchingDefinitions(serviceMatcher)
        showServiceTypes(matchingDefns, fileName, serviceMatcher.serviceNameinMessage)
      }

      Patch.empty
    }
  }

  // Collecting matching class/object definitions

  private def collectMatchingDefinitions(serviceMatcher: ServiceMatcher)(implicit doc: SemanticDocument): Seq[Defn] = {
    serviceMatcher match {
      case ServiceMatcherOnSuperType(_, _) =>
        collectMatchingDefinitionsMatchingOnSuperType(serviceMatcher)
    }
  }

  private def collectMatchingDefinitionsMatchingOnSuperType(
      serviceMatcher: ServiceMatcher
  )(implicit doc: SemanticDocument): Seq[Defn] = {
    val serviceTypeSymbolMatcher = serviceMatcher.symbols
      .map { sym =>
        SymbolMatcher.exact(sym.toString)
      }
      .reduce(_ + _)

    val classDefns: Seq[Defn.Class] = filterDescendantsOrSelf[Defn.Class](
      doc.tree,
      t =>
        getParentSymbolsOrSelf(t.symbol).exists { pt =>
          serviceTypeSymbolMatcher.matches(pt)
        } && !t.mods.exists(isAbstract)
    )

    val objectDefns: Seq[Defn.Object] = filterDescendantsOrSelf[Defn.Object](
      doc.tree,
      t =>
        getParentSymbolsOrSelf(t.symbol).exists { pt =>
          serviceTypeSymbolMatcher.matches(pt)
        } && !t.mods.exists(isAbstract)
    )

    classDefns.appendedAll(objectDefns)
  }

  // Showing the found definitions of "service types"

  private def showServiceTypes(definitions: Seq[Defn], fileName: Path, serviceNameInMessage: String)(implicit
      doc: SemanticDocument
  ): Unit = {
    definitions.foreach { defn =>
      println()
      println(s"Service implementation found in file '$fileName':")
      println(s"\tService type: $serviceNameInMessage")
      println(s"\tService implementation type found: ${defn.symbol}")

      println(s"\tSuper-types (or self):")
      getParentSymbolsOrSelf(defn.symbol).foreach { superTpe =>
        println(s"\t\t$superTpe")
      }

      println(s"\tPublic concrete declared methods:")
      getDeclaredMethodsOfClass(defn.symbol).filter(_.isPublic).filter(!_.isAbstract).foreach { method =>
        println(s"\t\t${method.symbol}")
      }

      println(s"\tProtected concrete declared methods:")
      getDeclaredMethodsOfClass(defn.symbol)
        .filter(m => m.isProtected || m.isProtectedThis || m.isProtectedWithin)
        .filter(!_.isAbstract)
        .foreach { method =>
          println(s"\t\t${method.symbol}")
        }
    }
  }

  // Helper methods

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

  private def getDeclaredMethodsOfClass(classSym: Symbol)(implicit doc: SemanticDocument): Seq[SymbolInformation] = {
    for {
      classSignature <- classSym.info.map(_.signature).toSeq.collect { case signature: ClassSignature => signature }
      decl <- classSignature.declarations
      if decl.isDef || decl.isMethod
    } yield {
      decl
    }
  }

  // Tree navigation support

  private def filterDescendantsOrSelf[A <: Tree: ClassTag](tree: Tree, p: A => Boolean): List[A] = {
    val optSelf: List[A] = List(tree).collect { case t: A if p(t) => t }
    // Recursive
    tree.children.flatMap(ch => filterDescendantsOrSelf[A](ch, p)).prependedAll(optSelf)
  }

}

object ShowEnterpriseServices {

  private val servletTypeSymbol: Symbol = Symbol("javax/servlet/http/HttpServlet#")

  private def checkClassSymbol(sym: Symbol)(implicit doc: SemanticDocument): Unit = {
    require(
      sym.info.exists(info => info.isClass || info.isTrait || info.isInterface),
      s"Not a class/trait/interface: $sym"
    )
    require(sym.info.exists(_.isPublic), s"Not a public class/trait/interface: $sym")
  }

  sealed trait ServiceMatcher {
    def symbols: Seq[Symbol]
    def serviceNameinMessage: String
  }

  object ServiceMatcher {

    // Mstching on at least one of the given super-types
    final case class ServiceMatcherOnSuperType(symbols: Seq[Symbol], serviceNameinMessage: String)
        extends ServiceMatcher

    def fromConfigEntry(configEntry: EnterpriseServiceConfigEntry)(implicit doc: SemanticDocument): ServiceMatcher = {
      require(
        EnterpriseServiceConfigEntry.knownEntryTypes.contains(configEntry.typeOfEntry),
        s"Unknown config entry type: '${configEntry.typeOfEntry}'"
      )

      configEntry.typeOfEntry match {
        case "HasSuperType" =>
          val symbols: Seq[Symbol] = configEntry.symbols.ensuring(_.nonEmpty).map { symbolString =>
            // Replacing ".." in HOCON by "#"
            symbolString.replace("..", "#").pipe(Symbol.apply)
          }
          symbols.foreach(checkClassSymbol)
          ServiceMatcherOnSuperType(symbols, configEntry.serviceNameinMessage)
        case _ =>
          sys.error(s"Unknown config entry type: '${configEntry.typeOfEntry}'")
      }
    }

    val default: ServiceMatcherOnSuperType =
      ServiceMatcherOnSuperType(List(servletTypeSymbol), "Servlet")

  }

}

final case class EnterpriseServiceConfigEntry(typeOfEntry: String, symbols: List[String], serviceNameinMessage: String)

object EnterpriseServiceConfigEntry {
  val knownEntryTypes: Set[String] = Set("HasSuperType") // Poor man's enumeration (limitation of metaconfig)

  // Mind the two dots below, that must be replaced to get the symbol
  val default: EnterpriseServiceConfigEntry =
    EnterpriseServiceConfigEntry("HasSuperType", List("javax/servlet/http/HttpServlet.."), "Servlet")

  implicit val surface: Surface[EnterpriseServiceConfigEntry] =
    metaconfig.generic.deriveSurface[EnterpriseServiceConfigEntry]

  implicit val decoder: ConfDecoder[EnterpriseServiceConfigEntry] = metaconfig.generic.deriveDecoder(default)
}

final case class EnterpriseServiceConfig(entries: List[EnterpriseServiceConfigEntry] = Nil) {
  def isEmpty: Boolean = entries.isEmpty
}

object EnterpriseServiceConfig {
  val default: EnterpriseServiceConfig = EnterpriseServiceConfig()
  implicit val surface: Surface[EnterpriseServiceConfig] = metaconfig.generic.deriveSurface[EnterpriseServiceConfig]
  implicit val decoder: ConfDecoder[EnterpriseServiceConfig] = metaconfig.generic.deriveDecoder(default)
}
