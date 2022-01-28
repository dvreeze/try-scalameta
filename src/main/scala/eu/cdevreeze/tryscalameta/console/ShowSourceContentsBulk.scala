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
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardCopyOption
import java.nio.file.StandardOpenOption

import scala.jdk.StreamConverters.StreamHasToScala
import scala.util.Try
import scala.util.Using
import scala.util.chaining.scalaUtilChainingOps

import org.scalafmt.interfaces.Scalafmt

/**
 * Like program ShowSourceContents, but calling it repeatedly, saving the outputs to files.
 *
 * For each sub-directory of the input directory, the equivalent of program ShowSourceContents is called, saving the
 * output to a corresponding sub-directory of the given output directory.
 *
 * @author
 *   Chris de Vreeze
 */
object ShowSourceContentsBulk {

  def main(args: Array[String]): Unit = {
    require(args.lengthIs >= 2, s"Usage: ShowSourceContentsBulk <input dir> <output dir> [ <relative source dir> ... ]")
    val inputDir: Path = new File(args(0)).toPath.ensuring(d => Files.isDirectory(d), s"Not a directory: '${args(0)}'")
    val outputDir: Path = new File(args(1)).toPath.pipe(d => Files.createDirectories(d))

    val relativeSourceDirs: Seq[Path] = args.toIndexedSeq
      .drop(2)
      .pipe(ps => if (ps.isEmpty) Seq("src/main/scala") else ps)
      .map(p => Paths.get(p))

    val scalafmt: Scalafmt = getScalafmt
    val scalafmtConfig: Path = getScalafmtConfig

    createOutputSources(inputDir, outputDir, relativeSourceDirs, scalafmt, scalafmtConfig)
  }

  def createOutputSources(
      inputDir: Path,
      outputDir: Path,
      relativeSourceDirs: Seq[Path],
      scalafmt: Scalafmt,
      scalafmtConfig: Path
  ): Unit = {
    val inputProjectDirs: Seq[Path] = Using.resource(Files.list(inputDir)) {
      _.toScala(Seq).filter(f => Files.isDirectory(f))
    }

    inputProjectDirs.map(d => createOutputSource(d, outputDir, relativeSourceDirs, scalafmt, scalafmtConfig))
  }

  def createOutputSource(
      inputProjectDir: Path,
      outputDir: Path,
      relativeSourceDirs: Seq[Path],
      scalafmt: Scalafmt,
      scalafmtConfig: Path
  ): Option[Path] = {
    require(relativeSourceDirs.nonEmpty, s"Missing relative source directory/directories")

    println(s"Processing 'project' '${inputProjectDir.getFileName}'")

    val sourceName: String = inputProjectDir.getFileName.toString.split('-').toIndexedSeq.map(_.capitalize).mkString

    val sourceDirs: Seq[Path] =
      relativeSourceDirs.map(d => inputProjectDir.resolve(d)).filter(d => Files.isDirectory(d))

    if (sourceDirs.isEmpty) {
      println(s"\tNo Scala source directories found")
      None
    } else {
      Try {
        val generatedSource: String =
          ShowSourceContents.generateSourceContents(sourceName, sourceDirs, scalafmt, scalafmtConfig)

        println(s"\tGenerated output source $sourceName.scala (${generatedSource.length} characters)")

        val projectDirName: Path = inputProjectDir.getFileName
        val projectOutputDir: Path = Files.createDirectories(outputDir.resolve(projectDirName))

        Files
          .write(
            projectOutputDir.resolve(Paths.get(s"$sourceName.scala")),
            generatedSource.getBytes(StandardCharsets.UTF_8),
            StandardOpenOption.CREATE
          )
          .pipe(Option(_))
      }.getOrElse {
        println(
          s"Could not create output for project ${inputProjectDir.getFileName}. " +
            "Did a package not match the directory structure? Was an input not a valid Scala 2.13 source?"
        )
        None
      }
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
