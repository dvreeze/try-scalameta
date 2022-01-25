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

import scala.meta.Ctor
import scala.meta.Decl
import scala.meta.Defn
import scala.meta.Init
import scala.meta.Lit
import scala.meta.Mod
import scala.meta.Name
import scala.meta.Self
import scala.meta.Source
import scala.meta.Stat
import scala.meta.Template
import scala.meta.Term
import scala.meta.Tree
import scala.meta.Type
import scala.meta.XtensionParseInputLike
import scala.meta.contrib._
import scala.util.chaining.scalaUtilChainingOps

import eu.cdevreeze.tryscalameta.support.QuerySupport._
import eu.cdevreeze.tryscalameta.support.VirtualFileSupport._

/**
 * Prints a "view" of Scala files found in one or more given source file root directories to the console. This output
 * (per Scala source) is conceptually a bit javap-like (using defaults), such that only public members are shown and
 * that method implementations are left out. The output looks like valid Scala, and is indeed syntactically parseable by
 * scalameta, but would not compile. The output can even be fed to Scalafmt (in sbt, use task scalafmtOnly)! After that,
 * the result is best shown in an editor with syntax highlighting (but without showing compilation errors), such as
 * gedit.
 *
 * @author
 *   Chris de Vreeze
 */
object ShowSourceContents {

  private val emptySelf: Self = Self(Name.Anonymous(), None)

  private val bodyPlaceholder: Term.Name = Term.Name("???")
  private val rhsPlaceholder: Term.Name = Term.Name("???")
  private val typeBodyPlaceholder: Type.Name = Type.Name("Some__Type")

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
          checkParentOfChildrenIsThis(source)
          require(source.parent.isEmpty)
          SourceWithPath(source, new File(f.path).toPath, sourceDir)
        }
      }
    }

    val newSources: Seq[SourceWithPath] = transformSources(sources)

    def removeRhs(stat: Stat): Stat =
      stat.transform { case t: Term.Assign => t.copy(rhs = rhsPlaceholder) }.asInstanceOf[Stat]

    val newSource: Source = Source(stats =
      List(
        Defn.Object(
          mods = Nil,
          name = Term.Name("CombinedSource"),
          templ = Template(
            early = Nil,
            inits = Nil,
            self = emptySelf,
            stats = newSources.map(addCommentsAsAnnots).flatMap(_.stats).map(removeRhs).toList
          )
        )
      )
    )

    println(newSource.syntax)
  }

  def transformSource(source: Source): Source = {
    // Note that a statement is either a definition, declaration, term or import. Here the latter two are ignored.
    // Note that we must also go inside packages (which are statements themselves), and not stop there.
    val newStats: List[Stat] = {
      source.stats.flatMap(_.findTopmostOrSelf[Stat](isDefnOrDecl)).flatMap {
        case defn: Defn => optionallyTransformDefn(defn)
        case decl: Decl => optionallyTransformDecl(decl)
        case _          => None
      }
    }

    source.copy(stats = newStats).tap(t => checkParentOfChildrenIsThis(t)).ensuring(_.parent.isEmpty)
  }

  private def transformSources(sources: Seq[SourceWithPath]): Seq[SourceWithPath] = {
    sources.map(transformSource)
  }

  private def transformSource(source: SourceWithPath): SourceWithPath = {
    SourceWithPath(transformSource(source.source), source.absolutePath, source.sourceRootDir)
  }

  private def optionallyTransformDefn(defn: Defn): Option[Defn] = {
    defn match {
      case defn: Defn.Trait if isPublic(defn.mods)  => Some(transformTraitDefn(defn))
      case defn: Defn.Class if isPublic(defn.mods)  => Some(transformClassDefn(defn))
      case defn: Defn.Object if isPublic(defn.mods) => Some(transformObjectDefn(defn))
      case defn: Defn.Def if isPublic(defn.mods)    => Some(transformDefDefn(defn))
      case defn: Defn.Val if isPublic(defn.mods)    => Some(transformValDefn(defn))
      case defn: Defn.Var if isPublic(defn.mods)    => Some(transformVarDefn(defn))
      case defn: Defn.Type if isPublic(defn.mods)   => Some(transformTypeDefn(defn))
      case _                                        => None
    }
  }

  private def optionallyTransformDecl(decl: Decl): Option[Decl] = {
    decl match {
      case decl: Decl.Def if isPublic(decl.mods)  => Some(transformDefDecl(decl))
      case decl: Decl.Val if isPublic(decl.mods)  => Some(transformValDecl(decl))
      case decl: Decl.Var if isPublic(decl.mods)  => Some(transformVarDecl(decl))
      case decl: Decl.Type if isPublic(decl.mods) => Some(transformTypeDecl(decl))
      case _                                      => None
    }
  }

  private def transformTraitDefn(defn: Defn.Trait): Defn.Trait = {
    assert(defn.templ.stats.forall(_.findFirstAncestor[Stat]().exists(_.isEqual(defn))))

    val newStats: List[Stat] = {
      defn.templ.findTopmost[Stat](isDefnOrDecl).flatMap {
        case defn: Defn => optionallyTransformDefn(defn)
        case decl: Decl => optionallyTransformDecl(decl)
        case t          => Option(t)
      }
    }

    defn.copy(templ = defn.templ.copy(stats = newStats, self = emptySelf))
  }

  private def transformClassDefn(defn: Defn.Class): Defn.Class = {
    assert(defn.templ.stats.forall(_.findFirstAncestor[Stat]().exists(_.isEqual(defn))))

    val newStats: List[Stat] = {
      defn.templ.findTopmost[Stat](isDefnOrDecl).flatMap {
        case defn: Defn => optionallyTransformDefn(defn)
        case decl: Decl => optionallyTransformDecl(decl)
        case t          => Option(t)
      }
    }

    defn.copy(templ = defn.templ.copy(stats = newStats, self = emptySelf), ctor = removeDefaultArgs(defn.ctor))
  }

  private def transformObjectDefn(defn: Defn.Object): Defn.Object = {
    assert(defn.templ.stats.forall(_.findFirstAncestor[Stat]().exists(_.isEqual(defn))))

    val newStats: List[Stat] = {
      defn.templ.findTopmost[Stat](isDefnOrDecl).flatMap {
        case defn: Defn => optionallyTransformDefn(defn)
        case decl: Decl => optionallyTransformDecl(decl)
        case t          => Option(t)
      }
    }

    defn.copy(templ = defn.templ.copy(stats = newStats, self = emptySelf))
  }

  private def transformDefDefn(defn: Defn.Def): Defn.Def = {
    defn
      .copy(body = bodyPlaceholder, mods = removeThrowsAnnot(defn.mods))
      .pipe(removeDefaultArgs)
  }

  private def transformValDefn(defn: Defn.Val): Defn.Val = {
    defn.copy(rhs = rhsPlaceholder)
  }

  private def transformVarDefn(defn: Defn.Var): Defn.Var = {
    defn.copy(rhs = Some(rhsPlaceholder))
  }

  private def transformTypeDefn(defn: Defn.Type): Defn.Type = {
    defn.copy(body = typeBodyPlaceholder)
  }

  private def transformTypeDecl(decl: Decl.Type): Decl.Type = {
    decl
  }

  private def transformDefDecl(decl: Decl.Def): Decl.Def = {
    decl
      .copy(mods = removeThrowsAnnot(decl.mods))
      .pipe(removeDefaultArgs)
  }

  private def transformValDecl(decl: Decl.Val): Decl.Val = {
    decl
  }

  private def transformVarDecl(decl: Decl.Var): Decl.Var = {
    decl
  }

  private def addCommentsAsAnnots(sourceWithPath: SourceWithPath): Source = {
    val commentString = s"File: ${sourceWithPath.relativePath}"

    sourceWithPath.source.copy(stats = sourceWithPath.source.stats.map(stat => addCommentAsAnnot(stat, commentString)))
  }

  private def addCommentAsAnnot(stat: Stat, comment: String): Stat = {
    stat match {
      case defn: Defn.Trait  => defn.copy(mods = addCommentAsAnnot(defn.mods, comment))
      case defn: Defn.Class  => defn.copy(mods = addCommentAsAnnot(defn.mods, comment))
      case defn: Defn.Object => defn.copy(mods = addCommentAsAnnot(defn.mods, comment))
      case defn: Defn.Def    => defn.copy(mods = addCommentAsAnnot(defn.mods, comment))
      case defn: Defn.Val    => defn.copy(mods = addCommentAsAnnot(defn.mods, comment))
      case defn: Defn.Var    => defn.copy(mods = addCommentAsAnnot(defn.mods, comment))
      case defn: Defn.Type   => defn.copy(mods = addCommentAsAnnot(defn.mods, comment))
      case decl: Decl.Def    => decl.copy(mods = addCommentAsAnnot(decl.mods, comment))
      case decl: Decl.Val    => decl.copy(mods = addCommentAsAnnot(decl.mods, comment))
      case decl: Decl.Var    => decl.copy(mods = addCommentAsAnnot(decl.mods, comment))
      case decl: Decl.Type   => decl.copy(mods = addCommentAsAnnot(decl.mods, comment))
      case t                 => t
    }
  }

  private def addCommentAsAnnot(mods: List[Mod], comment: String): List[Mod] = {
    mods.prepended(
      Mod.Annot(init =
        Init(tpe = Type.Name("comment"), name = Name.Anonymous(), argss = List(List(Lit.String(comment))))
      )
    )
  }

  private def isDefnOrDecl(stat: Stat): Boolean = stat match {
    case _: Defn | _: Decl => true
    case _                 => false
  }

  private def isPublic(mods: List[Mod]): Boolean = !mods.exists(_.isAccessMod)

  private def removeThrowsAnnot(mods: List[Mod]): List[Mod] = mods.filterNot {
    case Mod.Annot(Init(Type.Apply(Type.Name("throws"), _), _, _)) => true
    case _                                                         => false
  }

  private def removeDefaultArgs(ctor: Ctor.Primary): Ctor.Primary = {
    ctor.copy(paramss = ctor.paramss.map(_.map(removeDefaultArgs)))
  }

  private def removeDefaultArgs(decl: Decl.Def): Decl.Def = {
    decl.copy(paramss = decl.paramss.map(_.map(removeDefaultArgs)))
  }

  private def removeDefaultArgs(defn: Defn.Def): Defn.Def = {
    defn.copy(paramss = defn.paramss.map(_.map(removeDefaultArgs)))
  }

  private def removeDefaultArgs(param: Term.Param): Term.Param = param.copy(default = None)

  private def checkParentOfChildrenIsThis(tree: Tree): Unit = {
    tree.findAllDescendants[Tree]().foreach { tree =>
      require(
        tree.children.forall(_.parent.exists(_.isEqual(tree))),
        s"The parent of the children of some tree is not the tree"
      )
    }
  }
}
