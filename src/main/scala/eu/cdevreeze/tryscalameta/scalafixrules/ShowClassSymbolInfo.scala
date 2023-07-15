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

import scala.meta._
import scala.util.chaining.scalaUtilChainingOps

import scalafix.v1._

/**
 * Shows classes, traits and objects, and their corresponding symbol information data (except for implementations). The
 * outputs are syntactically Scala sources, stripped from implementations of methods etc., and annotated with metadata
 * extracted from the symbol information of the class/trait/object.
 *
 * This "rule" only depends on the Scala standard library and on Scalafix (and therefore Scalameta) and nothing else, so
 * this rule can easily be run from its source path against sbt or Maven projects.
 *
 * @author
 *   Chris de Vreeze
 */
final class ShowClassSymbolInfo extends SemanticRule("ShowClassSymbolInfo") {

  import ShowClassSymbolInfo.StripImplementation._

  private val MetadataAnnotType: Type = Type.Name("metadata")

  override def fix(implicit doc: SemanticDocument): Patch = {
    val defns: Seq[Defn] = doc.tree.collect {
      case defn: Defn.Class  => defn
      case defn: Defn.Trait  => defn
      case defn: Defn.Object => defn
    }

    val fileName: Path = doc.input.asInstanceOf[Input.VirtualFile].path.pipe(Paths.get(_)).getFileName

    println()
    println(
      s"Classes/traits/objects and their corresponding symbol information (ignoring implementations) in file $fileName:"
    )

    defns.foreach {
      case defn: Defn.Class =>
        println()
        println(showClass(defn.pipe(stripImplementation)))
      case defn: Defn.Trait =>
        println()
        println(showTrait(defn.pipe(stripImplementation)))
      case defn: Defn.Object =>
        println()
        println(showObject(defn.pipe(stripImplementation)))
    }

    Patch.empty
  }

  private def showClass(defn: Defn.Class)(implicit doc: SemanticDocument): String = {
    defn.symbol.info.get.ensuring(_.isClass).ensuring(_.isScala).pipe(info => annotateClass(defn, info).syntax)
  }

  private def annotateClass(defn: Defn.Class, info: SymbolInformation)(implicit doc: SemanticDocument): Defn.Class = {
    require(info.isClass, s"Not a class: $info")

    val metadata: Seq[Mod.Annot] = getClassMetadata(info)

    metadata.map(_.syntax).appended(defn.syntax).mkString("\n").pipe(_.parse[Stat].get.asInstanceOf[Defn.Class])
  }

  private def showTrait(defn: Defn.Trait)(implicit doc: SemanticDocument): String = {
    defn.symbol.info.get.ensuring(_.isTrait).ensuring(_.isScala).pipe(info => annotateTrait(defn, info).syntax)
  }

  private def annotateTrait(defn: Defn.Trait, info: SymbolInformation)(implicit doc: SemanticDocument): Defn.Trait = {
    require(info.isTrait, s"Not a trait: $info")

    val metadata: Seq[Mod.Annot] = getClassMetadata(info)

    metadata.map(_.syntax).appended(defn.syntax).mkString("\n").pipe(_.parse[Stat].get.asInstanceOf[Defn.Trait])
  }

  private def showObject(defn: Defn.Object)(implicit doc: SemanticDocument): String = {
    defn.symbol.info.get.ensuring(_.isObject).ensuring(_.isScala).pipe(info => annotateObject(defn, info).syntax)
  }

  private def annotateObject(defn: Defn.Object, info: SymbolInformation)(implicit
      doc: SemanticDocument
  ): Defn.Object = {
    require(info.isObject, s"Not an object: $info")

    val metadata: Seq[Mod.Annot] = getClassMetadata(info)

    metadata.map(_.syntax).appended(defn.syntax).mkString("\n").pipe(_.parse[Stat].get.asInstanceOf[Defn.Object])
  }

  private def getClassMetadata(info: SymbolInformation)(implicit doc: SemanticDocument): Seq[Mod.Annot] = {
    val signature: ClassSignature = info.signature.asInstanceOf[ClassSignature]

    Seq(
      createMetadataAnnot("symbol", info.symbol.toString),
      createMetadataAnnot("owner", info.owner.toString),
      createMetadataAnnot("displayName", info.displayName),
      createMetadataAnnot("isScala", info.isScala.toString),
      createMetadataAnnot("kind", getKind(info)),
      createMetadataAnnot("isAbstract", info.isAbstract.toString),
      createMetadataAnnot("isFinal", info.isFinal.toString),
      createMetadataAnnot("isSealed", info.isSealed.toString),
      createMetadataAnnot("isImplicit", info.isImplicit.toString),
      createMetadataAnnot("isCase", info.isCase.toString)
    )
      .appendedAll(info.overriddenSymbols.map(sym => createMetadataAnnot("overridden-symbol", sym.toString)))
      .appendedAll(signature.typeParameters.map(typePar => createMetadataAnnot("typeParameter", typePar.toString)))
      .appendedAll(signature.parents.map(parent => createMetadataAnnot("parent", parent.toString)))
      .appended(createMetadataAnnot("self", signature.self.toString))
      .appendedAll(signature.declarations.flatMap { decl =>
        decl.symbol.info.get.overriddenSymbols
          .map(sym => createMetadataAnnot("overridden-decl", sym.info.map(_.toString).getOrElse(sym.toString)))
          .prepended(createMetadataAnnot("decl-info", decl.symbol.info.get.toString))
      })
  }

  private def createMetadataAnnot(key: String, value: String): Mod.Annot = {
    Mod.Annot(
      Init(
        tpe = MetadataAnnotType,
        name = Name.Anonymous(),
        argClauses = List(List(Lit.String(key), Lit.String(value)))
      )
    )
  }

  private def getKind(info: SymbolInformation): String = info match {
    case info if info.isLocal         => "isLocal"
    case info if info.isField         => "isField"
    case info if info.isMethod        => "isMethod"
    case info if info.isConstructor   => "isConstructor"
    case info if info.isMacro         => "isMacro"
    case info if info.isType          => "isType"
    case info if info.isParameter     => "isParameter"
    case info if info.isSelfParameter => "isSelfParameter"
    case info if info.isTypeParameter => "isTypeParameter"
    case info if info.isObject        => "isObject"
    case info if info.isPackage       => "isPackage"
    case info if info.isPackageObject => "isPackageObject"
    case info if info.isClass         => "isClass"
    case info if info.isInterface     => "isInterface"
    case info if info.isTrait         => "isTrait"
  }

}

object ShowClassSymbolInfo {

  private object StripImplementation {

    private val termPlaceholder: Term.Name = Term.Name("???")

    def stripImplementation(defn: Defn.Class): Defn.Class = {
      defn.copy(templ = defn.templ.pipe(stripImplementation))
    }

    def stripImplementation(defn: Defn.Trait): Defn.Trait = {
      defn.copy(templ = defn.templ.pipe(stripImplementation))
    }

    def stripImplementation(defn: Defn.Object): Defn.Object = {
      defn.copy(templ = defn.templ.pipe(stripImplementation))
    }

    private def stripImplementation(templ: Template): Template = {
      templ.copy(early = templ.early.map(stripImplementation), stats = templ.stats.map(stripImplementation))
    }

    private def stripImplementation(stat: Stat): Stat = stat match {
      case defn: Defn.Def    => stripImplementation(defn)
      case defn: Defn.Val    => stripImplementation(defn)
      case defn: Defn.Var    => stripImplementation(defn)
      case defn: Defn.Class  => stripImplementation(defn)
      case defn: Defn.Trait  => stripImplementation(defn)
      case defn: Defn.Object => stripImplementation(defn)
      case stat              => stat
    }

    private def stripImplementation(defn: Defn.Def): Defn.Def = {
      defn.copy(body = termPlaceholder)
    }

    private def stripImplementation(defn: Defn.Val): Defn.Val = {
      defn.copy(rhs = termPlaceholder)
    }

    private def stripImplementation(defn: Defn.Var): Defn.Var = {
      defn.copy(rhs = Some(defn.body.pipe(_ => termPlaceholder)))
    }

  }

}
