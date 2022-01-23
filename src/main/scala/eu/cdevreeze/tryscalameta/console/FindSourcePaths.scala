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

import scala.jdk.StreamConverters.StreamHasToScala
import scala.util.Try
import scala.util.Using

/**
 * Given one or more source directories, creates a listing of all Scala sources in those source trees.
 *
 * The output can be stored in a file, say sources.txt. The idea is to be able to call commands scalac or metac
 * with an options file and a sources file. The latter can be generated by this program. Commands scalac and metac
 * can then be invoked by passing 2 arguments, namely the options file and sources file, both preceded by an ampersand.
 * This way of invoking scalac (and metac) is known from javac (the Java compiler).
 *
 * @author
 *   Chris de Vreeze
 */
object FindSourcePaths {

  def main(args: Array[String]): Unit = {
    require(args.lengthIs >= 1, s"Usage: FindSourcePaths <source dir> ...")

    val sourceDirs: Seq[Path] = args.toSeq.map(f => new File(f).toPath)
    val sources: Seq[Path] = sourceDirs.flatMap(dir => findAllScalaSourceFiles(dir))

    sources.foreach(source => println(source.toAbsolutePath.toString))
  }

  def findAllScalaSourceFiles(dir: Path): Seq[Path] = {
    findAllSourceFiles(dir, f => Try(f.getFileName.toString).getOrElse("").endsWith(".scala"))
  }

  def findAllSourceFiles(dir: Path, isSource: Path => Boolean): Seq[Path] = {
    require(Files.isDirectory(dir), s"Not a directory: $dir")

    findAllNormalFiles(dir).filter(isSource)
  }

  private def findAllNormalFiles(dir: Path): Seq[Path] = {
    require(Files.isDirectory(dir), s"Not a directory: $dir")
    // Does not follow symbolic links
    Using.resource(Files.walk(dir)) { _.toScala(Seq).filter(f => Files.isRegularFile(f)) }
  }
}
