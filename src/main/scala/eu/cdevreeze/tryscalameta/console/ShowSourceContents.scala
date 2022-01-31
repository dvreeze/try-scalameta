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
import java.nio.file.StandardCopyOption

import scala.meta._
import scala.meta.XtensionParseInputLike
import scala.meta.contrib._
import scala.util.chaining.scalaUtilChainingOps

import eu.cdevreeze.tryscalameta.support.QuerySupport._
import eu.cdevreeze.tryscalameta.support.VirtualFileSupport._
import org.scalafmt.interfaces.Scalafmt

/**
 * Prints a "view" of Scala files found in one or more given source file root directories to the console. This output
 * (per Scala source) is conceptually a bit javap-like (using defaults), such that only public members are shown and
 * that method implementations are left out. The output looks like valid Scala, and is indeed syntactically parseable by
 * scalameta, but would not compile. The output has been prettified by Scalafmt, using the Scalafmt config file on the
 * classpath. The result is best shown in an editor with syntax highlighting or in an IDE.
 *
 * The RHS terms of val and var definitions may occur completely or partly in the output, depending on their structure
 * and number of nodes in the syntax tree. The latter must not be greater than system property
 * "maxNodesInSimplifiedTree", or else the RHS in the output will be "???". The default value of
 * "maxNodesInSimplifiedTree" is 10. Similar remarks apply to the RHS types of type definitions, where the same system
 * property is used.
 *
 * @author
 *   Chris de Vreeze
 */
object ShowSourceContents {

  private val emptySelf: Self = Self(Name.Anonymous(), None)

  private val maxNodesInSimplifiedTree: Int = sys.props.getOrElse("maxNodesInSimplifiedTree", "10").toInt
  private val makeDefBodiesEmpty: Boolean = sys.props.getOrElse("makeDefBodiesEmpty", "true").toBoolean

  private val termPlaceholder: Term.Name = Term.Name("???")
  private val typePlaceholder: Type.Name = Type.Name("Some__Type")
  private val templatePlaceholder: Template = Template(Nil, Nil, emptySelf, Nil)

  private final case class SourceWithPath(source: Source, absolutePath: Path, sourceRootDir: Path) {
    require(Files.isDirectory(sourceRootDir), s"Not a directory: '$sourceRootDir")
    require(Files.isRegularFile(absolutePath), s"Not a regular file: '$absolutePath")
    require(absolutePath.startsWith(sourceRootDir), s"Path '$absolutePath' does not start with '$sourceRootDir'")

    def relativePath: Path = sourceRootDir.relativize(absolutePath)

    def packageMatchesRelativePath: Boolean = {
      val pkgs: Seq[Pkg] = source.stats.collect { case pkg: Pkg => pkg }

      if (pkgs.isEmpty) {
        true // Easy way out. Not checking for source without package.
      } else {
        val paths: Seq[Path] = pkgs.flatMap(pkg => Pkgs.convertToPaths(pkg))
        val relDirPath: Path = relativePath.getParent
        paths.exists(p => relDirPath.startsWith(p)) // Keeping package objects in mind
      }
    }

  }

  private object Pkgs {

    def convertToPaths(pkg: Pkg): Seq[Path] = {
      val firstPaths: Seq[Path] = convertToPath(pkg.ref).toSeq

      val nextPkgs: Seq[Pkg] = pkg.stats.collect { case subPkg: Pkg => subPkg }

      if (nextPkgs.isEmpty) {
        firstPaths
      } else {
        // Recursive calls
        val nextPaths: Seq[Path] = nextPkgs.flatMap(pkg => convertToPaths(pkg))
        firstPaths.flatMap(p1 => nextPaths.map(p2 => p1.resolve(p2)))
      }
    }

    def convertToPath(ref: Term.Ref): Option[Path] = {
      ref match {
        case t: Term.Name   => Some(Paths.get(t.value))
        case t: Term.Select =>
          // Recursive calls
          (t.qual match {
            case tq: Term.Ref => convertToPath(tq)
            case _            => None
          }).flatMap { path =>
            Some(path.resolve(Paths.get(t.name.value)))
          }
        case _ => None
      }
    }

  }

  def main(args: Array[String]): Unit = {
    require(args.lengthIs >= 2, s"Usage: ShowSourceContents <output source name> <source root dir> ...")

    val scalafmt: Scalafmt = getScalafmt
    val scalafmtConfig: Path = getScalafmtConfig

    val sourceName: String = args(0)
    val sourceDirs: Seq[Path] = args.toIndexedSeq.drop(1).map(arg => Paths.get(new File(arg).toURI))
    val source: String = generateSourceContents(sourceName, sourceDirs, scalafmt, scalafmtConfig)

    println(source)
  }

  def generateSourceContents(
      sourceName: String,
      sourceDirs: Seq[Path],
      scalafmt: Scalafmt,
      scalafmtConfig: Path
  ): String = {
    require(sourceDirs.nonEmpty, s"Missing source directory/directories")
    require(sourceDirs.forall(dir => Files.isDirectory(dir)), s"Not all passed paths are (source) directory paths")

    val unfilteredSources: Seq[SourceWithPath] = {
      sourceDirs.flatMap { sourceDir =>
        findAllScalaSourceFiles(sourceDir)
          .map { f =>
            val source: Source = f.parse[Source].get
            checkParentOfChildrenIsThis(source)
            require(source.parent.isEmpty)
            SourceWithPath(source, new File(f.path).toPath, sourceDir)
          }
      }
    }

    val sources: Seq[SourceWithPath] = unfilteredSources.filter(_.packageMatchesRelativePath)

    if (sources.sizeIs < unfilteredSources.size) {
      sys.error(s"Discrepancy between the package of at least one source and the relative directory structure")
    }

    val newSources: Seq[SourceWithPath] = transformSources(sources)

    val newSource: Source = Source(stats =
      List(
        Defn.Object(
          mods = Nil,
          name = Term.Name(sourceName),
          templ = Template(
            early = Nil,
            inits = Nil,
            self = emptySelf,
            stats = newSources.map(addCommentsAsAnnots).flatMap(_.stats).toList
          )
        )
      )
    )

    val formattedSource: String = scalafmt.format(scalafmtConfig, Paths.get(s"$sourceName.scala"), newSource.syntax)
    formattedSource
  }

  def transformSource(source: Source): Source = {
    // Note that a (non-package) statement is either a definition, declaration, term or import. Here the latter two are ignored.
    // Note that we must also go inside packages (which are statements themselves), and not stop at the packages.
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
    val body: Term = if (makeDefBodiesEmpty) termPlaceholder else simplifyTerm(defn.body)
    defn
      .copy(body = body, mods = removeThrowsAnnot(defn.mods))
      .pipe(removeDefaultArgs)
  }

  private def transformValDefn(defn: Defn.Val): Defn.Val = {
    defn.copy(rhs = simplifyTerm(defn.rhs))
  }

  private def transformVarDefn(defn: Defn.Var): Defn.Var = {
    defn.copy(rhs = defn.rhs.map(simplifyTerm))
  }

  private def transformTypeDefn(defn: Defn.Type): Defn.Type = {
    defn.copy(body = simplifyType(defn.body))
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

  private def simplifyTree(tree: Tree): Tree = {
    // Recursive top-down tree simplification, avoiding infinite recursion
    val transformer: Transformer = new Transformer {
      override def apply(tree: Tree): Tree = tree match {
        case t: Term.Block if t.stats.sizeIs >= 2 =>
          termPlaceholder
        case _: Term.Do | _: Term.For | _: Term.ForYield | _: Term.Match | _: Term.While =>
          termPlaceholder
        case _: Type.Match     => typePlaceholder
        case _: Template       => templatePlaceholder
        case t: Name.Anonymous => t
        case t: Term.Name      => t
        case t: Type.Name      => t
        case node              => super.apply(node)
      }
    }
    transformer(tree)
  }

  private def simplifyTerm(term: Term): Term = {
    simplifyTree(term).pipe { t =>
      if (t.findAllDescendantsOrSelf[Tree]().sizeIs <= maxNodesInSimplifiedTree) t.asInstanceOf[Term]
      else termPlaceholder
    }
  }

  private def simplifyType(tpe: Type): Type = {
    simplifyTree(tpe).pipe { t =>
      if (t.findAllDescendantsOrSelf[Tree]().sizeIs <= maxNodesInSimplifiedTree) t.asInstanceOf[Type]
      else typePlaceholder
    }
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

  private def getScalafmt: Scalafmt = {
    Scalafmt.create(this.getClass.getClassLoader)
  }

  private def getScalafmtConfig: Path = {
    val tempFilePath: Path = Files.createTempFile("scalafmt-", ".conf")
    // Works in a (JAR) ZIP file as well
    Files.copy(
      getClass.getClassLoader.getResourceAsStream("scalafmt.conf"),
      tempFilePath,
      StandardCopyOption.REPLACE_EXISTING
    )
    tempFilePath
  }

}
