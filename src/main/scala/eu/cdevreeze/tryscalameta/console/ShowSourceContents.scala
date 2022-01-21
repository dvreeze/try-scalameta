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

import scala.meta.Defn
import scala.meta.Mod
import scala.meta.Source
import scala.meta.Stat
import scala.meta.inputs.Input
import scala.util.chaining._

import eu.cdevreeze.tryscalameta.support.QuerySupport._
import eu.cdevreeze.tryscalameta.support.VirtualFileSupport._

/**
 * Prints a somewhat javap-like output about a given source file to the console.
 *
 * @author
 *   Chris de Vreeze
 */
object ShowSourceContents {

  def main(args: Array[String]): Unit = {
    require(args.sizeIs == 1, s"Missing source file path")

    val sourcePath: Path = Paths.get(new File(args(0)).toURI)
      .pipe(f => f.ensuring(Files.isRegularFile(f), "Not a normal file"))

    val sourceFile: Input.VirtualFile = makeVirtualFile(sourcePath)
    val source: Source = sourceFile.parse[Source].get

    // TODO Traits

    val classes = source.findAllTopmost[Defn.Class]()
      .filter(_.findFirstAncestor[Defn.Class]().isEmpty)
      .filter(_.findFirstAncestor[Defn.Object]().isEmpty)
    val objects = source.findAllTopmost[Defn.Object]()
      .filter(_.findFirstAncestor[Defn.Class]().isEmpty)
      .filter(_.findFirstAncestor[Defn.Object]().isEmpty)

    classes.foreach(printClassContents)
    objects.foreach(printObjectContents)
  }

  private def printClassContents(cls: Defn.Class): Unit = {
    val publicMethods: List[Defn.Def] = cls.findTopmost[Defn.Def](f => isPublic(f.mods))
      .filter(_.findFirstAncestor[Stat]().exists(_.structure == cls.structure))
    val publicVals: List[Defn.Val] = cls.findTopmost[Defn.Val](v => isPublic(v.mods))
      .filter(_.findFirstAncestor[Stat]().exists(_.structure == cls.structure))

    println(s"Class: ${cls.name}")

    publicMethods.foreach(m => println(s"\tPublic method: ${m.name}"))
    publicVals.foreach(v => println(s"\tPublic val: ${v.pats}"))
  }

  private def printObjectContents(obj: Defn.Object): Unit = {
    val publicMethods: List[Defn.Def] = obj.findTopmost[Defn.Def](f => isPublic(f.mods))
      .filter(_.findFirstAncestor[Stat]().exists(_.structure == obj.structure))
    val publicVals: List[Defn.Val] = obj.findTopmost[Defn.Val](v => isPublic(v.mods))
      .filter(_.findFirstAncestor[Stat]().exists(_.structure == obj.structure))

    println(s"Object: ${obj.name}")

    publicMethods.foreach(m => println(s"\tPublic method: ${m.name}"))
    publicVals.foreach(v => println(s"\tPublic val: ${v.pats}"))
  }

  private def isPublic(mods: List[Mod]): Boolean = {
    !mods.exists(_.isAccessMod)
  }
}
