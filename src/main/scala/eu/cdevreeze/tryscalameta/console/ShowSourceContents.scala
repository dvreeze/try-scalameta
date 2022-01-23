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

package eu.cdevreeze.tryscalameta.console

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.meta.Decl
import scala.meta.Defn
import scala.meta.Mod
import scala.meta.Name
import scala.meta.Self
import scala.meta.Source
import scala.meta.Stat
import scala.meta.XtensionQuasiquoteTerm
import scala.meta.XtensionQuasiquoteType
import scala.meta.contrib._

import eu.cdevreeze.tryscalameta.support.QuerySupport._
import eu.cdevreeze.tryscalameta.support.VirtualFileSupport._

/**
 * Prints a somewhat javap-like output (using Scala-like syntax) about one or more given directories of source files
 * to the console, ignoring non-public content.
 *
 * @author
 *   Chris de Vreeze
 */
object ShowSourceContents {

  private val deltaIndent = "  "
  private val emptySelf: Self = Self(Name(""), None)

  private final case class SourceWithPath(source: Source, absolutePath: Path, sourceRootDir: Path) {
    require(Files.isDirectory(sourceRootDir), s"Not a directory: '$sourceRootDir")
    require(Files.isRegularFile(absolutePath), s"Not a regular file: '$absolutePath")
    require(absolutePath.startsWith(sourceRootDir), s"Path '$absolutePath' does not start with '$sourceRootDir'")

    def relativePath: Path = sourceRootDir.relativize(absolutePath)
  }

  def main(args: Array[String]): Unit = {
    require(args.lengthIs >= 1, s"Missing source root directories. Usage: ShowSourceContents <source root dir> ...")

    val sourceDirs: Seq[Path] = args.map(arg => Paths.get(new File(arg).toURI))
    require(sourceDirs.forall(dir => Files.isDirectory(dir)), s"Not all passed paths are (source) directory paths")

    val sources: Seq[SourceWithPath] = {
      sourceDirs.flatMap { sourceDir =>
        findAllScalaSourceFiles(sourceDir).map { f =>
          val source: Source = f.parse[Source].get
          SourceWithPath(source, new File(f.path).toPath, sourceDir)
        }
      }
    }

    printSources(sources)
  }

  def printSources(sources: Seq[SourceWithPath]): Unit = {
    sources.foreach { sourceWithPath =>
      println()
      println(s"Source (in ${sourceWithPath.relativePath}):")
      println()
      printSource(sourceWithPath.source)
    }
  }

  def printSource(source: Source): Unit = {
    val newIndent = deltaIndent

    // Note that a statement is either a definition, declaration, term or import. Here the latter two are ignored.
    // Note that we must also go inside packages (which are statements themselves), and not stop there.
    source.stats.flatMap(_.findTopmostOrSelf[Stat](isDefnOrDecl)).foreach {
      case defn: Defn => printDefn(defn, newIndent)
      case decl: Decl => printDecl(decl, newIndent)
      case _          => ()
    }
  }

  private def printDefn(defn: Defn, indent: String): Unit = {
    defn match {
      case defn: Defn.Trait if isPublic(defn.mods)  => printTraitDefn(defn, indent)
      case defn: Defn.Class if isPublic(defn.mods)  => printClassDefn(defn, indent)
      case defn: Defn.Object if isPublic(defn.mods) => printObjectDefn(defn, indent)
      case defn: Defn.Def if isPublic(defn.mods)    => printDefDefn(defn, indent)
      case defn: Defn.Val if isPublic(defn.mods)    => printValDefn(defn, indent)
      case defn: Defn.Var if isPublic(defn.mods)    => printVarDefn(defn, indent)
      case defn: Defn.Type if isPublic(defn.mods)   => printTypeDefn(defn, indent)
      case _                                        => ()
    }
  }

  private def printDecl(decl: Decl, indent: String): Unit = {
    decl match {
      case decl: Decl.Def if isPublic(decl.mods)  => printDefDecl(decl, indent)
      case decl: Decl.Val if isPublic(decl.mods)  => printValDecl(decl, indent)
      case decl: Decl.Var if isPublic(decl.mods)  => printVarDecl(decl, indent)
      case decl: Decl.Type if isPublic(decl.mods) => printTypeDecl(decl, indent)
      case _                                      => ()
    }
  }

  private def printTraitDefn(defn: Defn.Trait, indent: String): Unit = {
    val newIndent = indent + deltaIndent

    assert(defn.templ.stats.forall(_.findFirstAncestor[Stat]().exists(_.isEqual(defn))))

    println(indent + defn.copy(templ = defn.templ.copy(stats = Nil, self = emptySelf)).syntax + ":")

    defn.templ.findTopmost[Stat](isDefnOrDecl).foreach {
      case defn: Defn => printDefn(defn, newIndent)
      case decl: Decl => printDecl(decl, newIndent)
      case _          => ()
    }
  }

  private def printClassDefn(defn: Defn.Class, indent: String): Unit = {
    val newIndent = indent + deltaIndent

    assert(defn.templ.stats.forall(_.findFirstAncestor[Stat]().exists(_.isEqual(defn))))

    println(indent + defn.copy(templ = defn.templ.copy(stats = Nil, self = emptySelf)).syntax + ":")

    defn.templ.findTopmost[Stat](isDefnOrDecl).foreach {
      case defn: Defn => printDefn(defn, newIndent)
      case decl: Decl => printDecl(decl, newIndent)
      case _          => ()
    }
  }

  private def printObjectDefn(defn: Defn.Object, indent: String): Unit = {
    val newIndent = indent + deltaIndent

    assert(defn.templ.stats.forall(_.findFirstAncestor[Stat]().exists(_.isEqual(defn))))

    println(indent + defn.copy(templ = defn.templ.copy(stats = Nil, self = emptySelf)).syntax + ":")

    defn.templ.findTopmost[Stat](isDefnOrDecl).foreach {
      case defn: Defn => printDefn(defn, newIndent)
      case decl: Decl => printDecl(decl, newIndent)
      case _          => ()
    }
  }

  private def printDefDefn(defn: Defn.Def, indent: String): Unit = {
    println(indent + defn.copy(body = q"body_placeholder", mods = removeThrowsAnnot(defn.mods)).syntax)
  }

  private def printValDefn(defn: Defn.Val, indent: String): Unit = {
    println(indent + defn.copy(rhs = q"rhs_placeholder").syntax)
  }

  private def printVarDefn(defn: Defn.Var, indent: String): Unit = {
    println(indent + defn.copy(rhs = Some(q"rhs_placeholder")).syntax)
  }

  private def printTypeDefn(defn: Defn.Type, indent: String): Unit = {
    println(indent + defn.copy(body = t"body_placeholder").syntax)
  }

  private def printTypeDecl(decl: Decl.Type, indent: String): Unit = {
    println(indent + decl.syntax)
  }

  private def printDefDecl(decl: Decl.Def, indent: String): Unit = {
    println(indent + decl.copy(mods = removeThrowsAnnot(decl.mods)).syntax)
  }

  private def printValDecl(decl: Decl.Val, indent: String): Unit = {
    println(indent + decl.syntax)
  }

  private def printVarDecl(decl: Decl.Var, indent: String): Unit = {
    println(indent + decl.syntax)
  }

  private def isDefnOrDecl(stat: Stat): Boolean = stat match {
    case _: Defn | _: Decl => true
    case _                 => false
  }

  private def isPublic(mods: List[Mod]): Boolean = !mods.exists(_.isAccessMod)

  private def removeThrowsAnnot(mods: List[Mod]): List[Mod] = mods.filterNot {
    case m: Mod.Annot if m.init.name.value == "throws" => true
    case _ => false
  }
}
