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
import scala.util.Try
import scala.util.chaining.scalaUtilChainingOps

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

  import ShowEnterpriseServices.DefinitionResult
  import ShowEnterpriseServices.FoundSymbols
  import ShowEnterpriseServices.ServiceDefinitionCollector
  import ShowEnterpriseServices.getDeclaredMethodsOfClass
  import ShowEnterpriseServices.getOptionalPrimaryConstructor
  import ShowEnterpriseServices.getParentSymbolsOrSelf

  def this() = this(EnterpriseServiceConfig.default)

  override def withConfiguration(config: Configuration): Configured[Rule] = {
    config.conf
      .getOrElse("ShowEnterpriseServices")(this.config)
      .map(newConfig => new ShowEnterpriseServices(newConfig))
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    if (config.isEmpty) {
      println("Missing \"enterprise service definition collectors\". Doing nothing.")
      Patch.empty
    } else {
      val serviceDefinitionCollectors: Seq[ServiceDefinitionCollector] =
        config.entries.flatMap(entry => ServiceDefinitionCollector.tryFromConfigEntry(entry).toOption)

      val fileName: Path = doc.input.asInstanceOf[Input.VirtualFile].path.pipe(Paths.get(_)).getFileName

      val matchingServletDefnResults: Seq[DefinitionResult] =
        ServiceDefinitionCollector.default.collectServiceDefinitions(doc)

      val otherMatchingDefnResults: Seq[DefinitionResult] =
        serviceDefinitionCollectors.flatMap { serviceDefinitionCollector =>
          serviceDefinitionCollector.collectServiceDefinitions(doc)
        }

      val allDefnResults: Seq[DefinitionResult] = matchingServletDefnResults.appendedAll(otherMatchingDefnResults)

      val defnResultsBySymbol: Map[String, Seq[DefinitionResult]] =
        allDefnResults.groupBy(_.defn.symbol.value)

      defnResultsBySymbol.foreach { case (_, defns) =>
        val foundSymbolGroups: Seq[FoundSymbols] = defns.flatMap(_.foundSymbolsOption.toSeq)

        showServiceTypes(defns.head.defn, fileName, defns.map(_.serviceDisplayName), foundSymbolGroups)
      }

      Patch.empty
    }
  }

  // Showing the found definitions of "service types"

  private def showServiceTypes(
      defn: Defn,
      fileName: Path,
      serviceDisplayNames: Seq[String],
      foundSymbolGroups: Seq[FoundSymbols]
  )(implicit
      doc: SemanticDocument
  ): Unit = {
    println()
    println(s"Service implementation found in file '$fileName':")
    serviceDisplayNames.foreach(serviceDisplayName => println(s"\tService type: $serviceDisplayName"))
    println(s"\tService implementation type found: ${defn.symbol}")

    foundSymbolGroups.foreach { foundSymbols =>
      println(s"\tFound symbols. Description: ${foundSymbols.description}. Symbols:")
      foundSymbols.symbols.foreach(sym => println(s"\t\t$sym"))
    }

    println(s"\tSuper-types (or self):")
    getParentSymbolsOrSelf(defn.symbol).foreach { superTpe =>
      println(s"\t\t$superTpe")
    }

    getOptionalPrimaryConstructor(defn.symbol).foreach { primaryConstructor =>
      println(s"\tPrimary constructor parameters (across parameter lists):")

      primaryConstructor.parameterLists.flatten.foreach { par =>
        println(s"\t\tConstructor parameter: ${par.symbol}")

        val parTypeOption: Option[Symbol] = Option(par.signature).collect { case ValueSignature(TypeRef(_, sym, _)) =>
          sym
        }
        parTypeOption.foreach { parType =>
          println(s"\t\t\tParameter type (raw): $parType")
        }
      }
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

object ShowEnterpriseServices {

  private val servletTypeSymbol: Symbol = Symbol("javax/servlet/http/HttpServlet#")

  final case class FoundSymbols(description: String, symbols: Seq[Symbol]) {

    def plusSymbol(sym: Symbol): FoundSymbols = {
      if (symbols.contains(sym)) {
        this
      } else {
        FoundSymbols(description, this.symbols.appended(sym))
      }
    }

  }

  final case class DefinitionResult(defn: Defn, serviceDisplayName: String, foundSymbolsOption: Option[FoundSymbols])

  sealed trait ServiceDefinitionCollector {

    /**
     * The symbols used for matching in the implementation of this service definition collector. Don't confuse these
     * with found symbols in definition results, which are unrelated.
     */
    def symbols: Seq[Symbol]

    /**
     * A "pretty" name for the (kind of) service, such as "Servlet" or "Kafka event processor".
     */
    def serviceDisplayName: String

    /**
     * Collects the matching service definitions occurring in the passed semantic document's syntax tree
     */
    def collectServiceDefinitions(doc: SemanticDocument): Seq[DefinitionResult]

    /**
     * The symbols turned into a SymbolMatcher, used by the implementation of this service definition collector
     */
    final def toSymbolMatcher: SymbolMatcher = {
      symbols.map(sym => SymbolMatcher.exact(sym.toString)).reduce(_ + _)
    }

  }

  object ServiceDefinitionCollector {

    /**
     * Service definition collector matching on at least one of the given super-types
     */
    final class ServiceDefinitionCollectorBasedOnSuperType(val symbols: Seq[Symbol], val serviceDisplayName: String)
        extends ServiceDefinitionCollector {

      override def collectServiceDefinitions(doc: SemanticDocument): Seq[DefinitionResult] = {
        implicit val semanticDoc: SemanticDocument = doc

        val serviceTypeSymbolMatcher = this.toSymbolMatcher

        val classDefns: Seq[Defn.Class] = filterDescendants[Defn.Class](
          doc.tree,
          t =>
            getParentSymbolsOrSelf(t.symbol).exists { pt =>
              serviceTypeSymbolMatcher.matches(pt)
            } && !t.mods.exists(isAbstract)
        )

        val objectDefns: Seq[Defn.Object] = filterDescendants[Defn.Object](
          doc.tree,
          t =>
            getParentSymbolsOrSelf(t.symbol).exists { pt =>
              serviceTypeSymbolMatcher.matches(pt)
            } && !t.mods.exists(isAbstract)
        )

        classDefns.appendedAll(objectDefns).map(defn => DefinitionResult(defn, serviceDisplayName, None))
      }

    }

    /**
     * Service definition collector matching on at least one of the given exact types as used type
     */
    final class ServiceDefinitionCollectorBasedOnUsedType(val symbols: Seq[Symbol], val serviceDisplayName: String)
        extends ServiceDefinitionCollector {

      override def collectServiceDefinitions(doc: SemanticDocument): Seq[DefinitionResult] = {
        implicit val semanticDoc: SemanticDocument = doc

        val symbolMatcher = this.toSymbolMatcher

        val matchingTerms: Seq[Term] = filterDescendants[Term](
          doc.tree,
          { t =>
            // Matching terms (matching the term's symbol owner) must be found within a function, and not within
            // imports or as a package, for example
            symbolMatcher.matches(t.symbol.owner) && findFirstAncestor[Defn.Def](t, _ => true).nonEmpty
          }
        )

        val classOrObjectDefns: Seq[Defn] =
          matchingTerms.flatMap { t =>
            findFirstAncestor[Defn with Stat.WithMods](
              t,
              anc => (anc.isInstanceOf[Defn.Class] || anc.isInstanceOf[Defn.Object]) && !anc.mods.exists(isAbstract)
            )
          }.distinct

        classOrObjectDefns.map(defn => DefinitionResult(defn, serviceDisplayName, None))
      }

    }

    /**
     * Service definition collector matching on at least one of the given exact methods as used methods
     */
    final class ServiceDefinitionCollectorBasedOnUsedMethod(val symbols: Seq[Symbol], val serviceDisplayName: String)
        extends ServiceDefinitionCollector {

      override def collectServiceDefinitions(doc: SemanticDocument): Seq[DefinitionResult] = {
        implicit val semanticDoc: SemanticDocument = doc

        val symbolMatcher = this.toSymbolMatcher

        val matchingTerms: Seq[Term] = filterDescendants[Term](
          doc.tree,
          { t =>
            // Matching terms must be found within a function, and not within imports or as a package, for example
            symbolMatcher.matches(t.symbol) && findFirstAncestor[Defn.Def](t, _ => true).nonEmpty
          }
        )

        val classOrObjectDefns: Seq[Defn] =
          matchingTerms.flatMap { t =>
            findFirstAncestor[Defn with Stat.WithMods](
              t,
              anc => (anc.isInstanceOf[Defn.Class] || anc.isInstanceOf[Defn.Object]) && !anc.mods.exists(isAbstract)
            )
          }.distinct

        classOrObjectDefns.map(defn => DefinitionResult(defn, serviceDisplayName, None))
      }

    }

    /**
     * Service definition collector matching on at least one of the given types or one of their sub-types as used type
     */
    final class ServiceDefinitionCollectorBasedOnUsedTypeOrSubType(
        val symbols: Seq[Symbol],
        val serviceDisplayName: String
    ) extends ServiceDefinitionCollector {

      override def collectServiceDefinitions(doc: SemanticDocument): Seq[DefinitionResult] = {
        implicit val semanticDoc: SemanticDocument = doc

        val symbolMatcher = this.toSymbolMatcher

        // For performance

        val matchingSymbolOwners: Set[Symbol] =
          filterDescendants[Term](doc.tree, _.symbol.info.exists(s => s.isDef || s.isMethod))
            .map(_.symbol.owner)
            .toSet
            .filter(so => getParentSymbolsOrSelf(so).exists(ps => symbolMatcher.matches(ps)))

        val matchingTerms: Seq[Term] = filterDescendants[Term](
          doc.tree,
          { t =>
            // Matching terms (matching the term's symbol owner) must be found within a function, and not within
            // imports or as a package, for example
            matchingSymbolOwners.contains(t.symbol.owner) && findFirstAncestor[Defn.Def](t, _ => true).nonEmpty
          }
        )

        val foundSymbols: FoundSymbols = FoundSymbols(
          s"Found used sub-types or self of [ ${symbols.mkString(", ")} ]",
          matchingTerms.map(_.symbol.owner).distinct
        )

        val classOrObjectDefns: Seq[Defn] =
          matchingTerms.flatMap { t =>
            findFirstAncestor[Defn with Stat.WithMods](
              t,
              anc => (anc.isInstanceOf[Defn.Class] || anc.isInstanceOf[Defn.Object]) && !anc.mods.exists(isAbstract)
            )
          }.distinct

        classOrObjectDefns.map(defn => DefinitionResult(defn, serviceDisplayName, Some(foundSymbols)))
      }

    }

    /**
     * Service definition collector matching on at least one of the given exact types as used annotation type
     */
    final class ServiceDefinitionCollectorBasedOnUsedAnnotation(
        val symbols: Seq[Symbol],
        val serviceDisplayName: String
    ) extends ServiceDefinitionCollector {

      override def collectServiceDefinitions(doc: SemanticDocument): Seq[DefinitionResult] = {
        implicit val semanticDoc: SemanticDocument = doc

        val symbolMatcher = this.toSymbolMatcher

        val matchingDefns: Seq[Defn with Stat.WithMods] = filterDescendants[Defn with Stat.WithMods](
          doc.tree,
          { t =>
            t.mods.exists(mod => isAnnot(mod, symbolMatcher))
          }
        )

        val matchingDecls: Seq[Decl with Stat.WithMods] = filterDescendants[Decl with Stat.WithMods](
          doc.tree,
          { t =>
            t.mods.exists(mod => isAnnot(mod, symbolMatcher))
          }
        )

        val classOrObjectDefns: Seq[Defn] =
          matchingDefns
            .appendedAll(matchingDecls)
            .flatMap { t =>
              findFirstAncestorOrSelf[Defn with Stat.WithMods](
                t,
                anc => (anc.isInstanceOf[Defn.Class] || anc.isInstanceOf[Defn.Object]) && !anc.mods.exists(isAbstract)
              )
            }
            .distinct

        classOrObjectDefns.map(defn => DefinitionResult(defn, serviceDisplayName, None))
      }

    }

    def tryFromConfigEntry(
        configEntry: EnterpriseServiceConfigEntry
    )(implicit doc: SemanticDocument): Try[ServiceDefinitionCollector] = {
      require(
        EnterpriseServiceConfigEntry.knownEntryTypes.contains(configEntry.typeOfEntry),
        s"Unknown config entry type: '${configEntry.typeOfEntry}'"
      )

      val symbols: Seq[Symbol] = configEntry.symbols.ensuring(_.nonEmpty).map { symbolString =>
        // Replacing ".." in HOCON by "#"
        symbolString.replace("..", "#").pipe(Symbol.apply)
      }

      // Checks below may fail if the symbol is unknown due to it not being on the classpath

      configEntry.typeOfEntry match {
        case "HasSuperType" =>
          Try(symbols.foreach(checkClassSymbol)).flatMap { _ =>
            Try(new ServiceDefinitionCollectorBasedOnSuperType(symbols, configEntry.serviceDisplayName))
          }
        case "UsesType" =>
          Try(symbols.foreach(checkClassSymbol)).flatMap { _ =>
            Try(new ServiceDefinitionCollectorBasedOnUsedType(symbols, configEntry.serviceDisplayName))
          }
        case "UsesMethod" =>
          Try(symbols.foreach(checkMethodSymbol)).flatMap { _ =>
            Try(new ServiceDefinitionCollectorBasedOnUsedMethod(symbols, configEntry.serviceDisplayName))
          }
        case "UsesTypeOrSubType" =>
          Try(symbols.foreach(checkClassSymbol)).flatMap { _ =>
            Try(new ServiceDefinitionCollectorBasedOnUsedTypeOrSubType(symbols, configEntry.serviceDisplayName))
          }
        case "UsesAnnotation" =>
          Try(symbols.foreach(checkClassSymbol)).flatMap { _ =>
            Try(new ServiceDefinitionCollectorBasedOnUsedAnnotation(symbols, configEntry.serviceDisplayName))
          }
        case _ =>
          sys.error(s"Unknown config entry type: '${configEntry.typeOfEntry}'")
      }
    }

    val default: ServiceDefinitionCollectorBasedOnSuperType =
      new ServiceDefinitionCollectorBasedOnSuperType(List(servletTypeSymbol), "Servlet")

  }

  // Helper methods

  private def checkClassSymbol(sym: Symbol)(implicit doc: SemanticDocument): Unit = {
    require(
      sym.info.exists(info => info.isClass || info.isTrait || info.isInterface),
      s"Not a class/trait/interface: $sym"
    )
    require(sym.info.exists(_.isPublic), s"Not a public class/trait/interface: $sym")
  }

  private def checkMethodSymbol(sym: Symbol)(implicit doc: SemanticDocument): Unit = {
    require(
      sym.info.exists(info => info.isMethod),
      s"Not a method: $sym"
    )
    require(sym.info.exists(_.isPublic), s"Not a public method: $sym")
  }

  // See https://github.com/scalameta/scalameta/issues/467
  private def isAbstract(mod: Mod): Boolean = mod match {
    case mod"abstract" => true
    case _             => false
  }

  // See https://github.com/scalameta/scalameta/issues/467
  private def isAnnot(mod: Mod, symbolMatcher: SymbolMatcher)(implicit doc: SemanticDocument): Boolean = mod match {
    case mod"@$annot" =>
      annot match {
        case Mod.Annot(Init.After_4_6_0(tpe, _, _)) => symbolMatcher.matches(tpe.symbol)
        case _                                      => false
      }
    case _ => false
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

  private def getOptionalPrimaryConstructor(
      classSym: Symbol
  )(implicit doc: SemanticDocument): Option[MethodSignature] = {
    (for {
      classSignature <- classSym.info.map(_.signature).toSeq.collect { case signature: ClassSignature => signature }
      decl <- classSignature.declarations
      if decl.isPrimary
      constructorSignature <- Seq(decl.signature).collect { case signature: MethodSignature => signature }
    } yield {
      constructorSignature
    }).headOption
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

  private def findFirstAncestor[A <: Tree: ClassTag](tree: Tree, p: A => Boolean): Option[A] =
    filterAncestors[A](tree, p).headOption

  private def findFirstAncestorOrSelf[A <: Tree: ClassTag](tree: Tree, p: A => Boolean): Option[A] =
    filterAncestorsOrSelf[A](tree, p).headOption

}

final case class EnterpriseServiceConfigEntry(typeOfEntry: String, symbols: List[String], serviceDisplayName: String)

object EnterpriseServiceConfigEntry {

  val knownEntryTypes: Set[String] =
    Set(
      "HasSuperType",
      "UsesType",
      "UsesMethod",
      "UsesTypeOrSubType",
      "UsesAnnotation"
    ) // Poor man's enumeration (limitation of metaconfig)

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
