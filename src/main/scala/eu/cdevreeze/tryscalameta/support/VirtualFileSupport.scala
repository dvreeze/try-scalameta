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

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

import scala.meta.inputs.Input
import scala.util.Try

/**
 * Virtual file support.
 *
 * @author
 *   Chris de Vreeze
 */
object VirtualFileSupport {

  private val utf8 = StandardCharsets.UTF_8.toString

  def makeVirtualFile(path: Path): Input.VirtualFile = {
    require(Files.isRegularFile(path), s"Not a regular file: $path")
    val bytes: Array[Byte] = Files.readAllBytes(path)
    val text: String = new String(bytes, utf8)
    Input.VirtualFile(path.toString, text)
  }

  def findAllSourceFiles(dir: Path, isSource: Path => Boolean): Seq[Input.VirtualFile] = {
    require(dir.toFile.isDirectory, s"Not a directory: $dir")

    findAllNormalFiles(dir.toFile).map(_.toPath).filter(isSource).map(makeVirtualFile)
  }

  def findAllScalaSourceFiles(dir: Path): Seq[Input.VirtualFile] = {
    findAllSourceFiles(dir, f => Try(f.getFileName.toString).getOrElse("").endsWith(".scala"))
  }

  // TODO Use nio.file.Files instead
  private def findAllNormalFiles(dir: File): Seq[File] = {
    require(dir.isDirectory, s"Not a directory: $dir")

    dir.listFiles().toSeq.flatMap {
      case d: File if d.isDirectory =>
        // Recursive call
        findAllNormalFiles(d)
      case f: File if f.isFile =>
        Seq(f)
      case _: File =>
        Seq.empty
    }
  }
}
