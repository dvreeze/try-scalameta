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

import scala.io.Codec
import scala.util.Try
import scala.util.Using
import scala.util.control.NonFatal

import scopt.OParser
import scopt.OParserBuilder

/**
 * Runs a shell command repeatedly, once for each sub-directory of a given root directory. A subdir filter file can be
 * used to filter the sub-directories for which to run the command.
 *
 * The shell command can assume as current directory the sub-directory for which it is running at that moment. Make sure
 * the command is runnable. Consider making it a script starting with a sub-command printing the current directory.
 *
 * For the command line options and arguments, run the program with "--help".
 *
 * @author
 *   Chris de Vreeze
 */
object RunForSubDirectories {

  def main(args: Array[String]): Unit = {
    OParser.parse(createCommandLineParser(), args, Config()) match {
      case Some(cfg) =>
        runCommands(cfg.rootDir.toPath, cfg.subdirFilterFile.map(_.toPath), cfg.outputFile.toPath, cfg.command)
      case None =>
        () // Error message has already been displayed
    }
  }

  def runCommands(
      rootDir: Path,
      subdirFilterFile: Option[Path],
      outputFile: Path,
      command: Seq[String]
  ): Unit = {
    val subdirNames: Seq[Path] =
      subdirFilterFile
        .map { f =>
          Using.resource(scala.io.Source.fromFile(f.toFile)(Codec.UTF8)) { _.getLines().toSeq.map(n => Paths.get(n)) }
        }
        .getOrElse {
          rootDir.toFile.listFiles(_.isDirectory).toSeq.map(_.toPath)
        }

    Files.createFile(outputFile)

    subdirNames.foreach { subdir =>
      println(s"Running command '${command.mkString(" ")}' for directory '${rootDir.resolve(subdir)}'")
      runCommandForSubdir(rootDir, subdir, outputFile, command)
    }
  }

  def runCommandForSubdir(rootDir: Path, subdirName: Path, outputFile: Path, command: Seq[String]): Unit = {
    Try {
      val pb: ProcessBuilder = createProcessBuilder(rootDir.resolve(subdirName), outputFile, command)

      val process: Process = pb.start()
      process.waitFor()
    }.recover { case NonFatal(t) =>
      Console.err.println(s"Command failed for sub-directory '${rootDir.resolve(subdirName)}'")
      Console.err.println(s"Thrown exception: $t")
    }.get
  }

  private def createProcessBuilder(
      absoluteSubdir: Path,
      outputFile: Path,
      command: Seq[String]
  ): ProcessBuilder = {
    val pb = new ProcessBuilder(command: _*)
    pb.directory(absoluteSubdir.toFile) // current working directory
    pb.redirectOutput(ProcessBuilder.Redirect.appendTo(outputFile.toFile))
    pb
  }

  private def createCommandLineParser(): OParser[_, Config] = {
    val builder: OParserBuilder[Config] = OParser.builder[Config]

    import builder._

    OParser.sequence(
      programName("run-for-subdirectories"),
      head("run-for-subdirectories"),
      opt[File]('r', "root")
        .required()
        .valueName("<root dir>")
        .action { (v, cfg) => cfg.copy(rootDir = v) }
        .text("required root directory (as parent of the sub-directories)")
        .validate { f =>
          if (Files.isDirectory(f.toPath)) success else failure("Value <root dir> must be an existing directory")
        },
      opt[File]("subdirs")
        .optional()
        .valueName("<file with subdirs>")
        .action { (v, cfg) => cfg.copy(subdirFilterFile = Some(v)) }
        .text("optional file containing newline-separated subdirs (relative to the root directory)")
        .validate { f =>
          if (Files.isRegularFile(f.toPath)) success
          else failure("Optional value <file with subdirs> must be an existing file")
        },
      opt[File]('o', "output")
        .required()
        .valueName("<output file>")
        .action { (v, cfg) => cfg.copy(outputFile = v) }
        .text("required output file path")
        .validate { f =>
          if (f.exists) failure("Value <output file> must be a yet non-existing file") else success
        },
      help("help").text("prints this usage text"),
      arg[String]("<cmd> <cmd par>...")
        .unbounded()
        .required()
        .valueName("<command parts>")
        .action { (v, cfg) => cfg.copy(command = cfg.command.appended(v)) }
        .validate(cmd =>
          if (cmd.isEmpty) failure("Value <command parts> must be non-empty, containing at least a command")
          else success
        )
    )
  }

  private final case class Config(
      rootDir: File = Paths.get(".").toFile,
      subdirFilterFile: Option[File] = None,
      outputFile: File = Paths.get(".").toFile,
      command: Seq[String] = Seq.empty
  )

}
