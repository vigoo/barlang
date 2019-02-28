package io.github.vigoo.barlang

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}

import io.github.vigoo.barlang.compiler._
import io.github.vigoo.barlang.language.{Parser => BarlangParser}
import io.github.vigoo.clipp.parsers._
import io.github.vigoo.clipp.syntax._
import io.github.vigoo.clipp.usageinfo.{UsageInfoExtractor, UsagePrettyPrinter}
import io.github.vigoo.clipp.{Parser, errors}

import scala.io.Source

object Cli {

  sealed trait Command
  final case class Compile(source: File,
                           target: Option[File]) extends Command

  def main(args: Array[String]): Unit = {
    val spec = for {
      cmd <- command("compile")
      result <- cmd match {
        case "compile" =>
          for {
            source <- parameter[File]("source file", "barlang file")
            target <- optional { parameter[File]("target file", "bash file")}
          } yield Compile(source, target)
      }
    } yield result

    Parser.extractParameters(args, spec) match {
      case Left(error) =>
        println(errors.display(error.errors))

        val usageGraph = UsageInfoExtractor.getUsageDescription(spec, error.partialChoices)
        println(UsagePrettyPrinter.prettyPrint(usageGraph))

      case Right(Compile(source, target)) =>
        BarlangParser.BarlangLexer.parse(BarlangParser.BarlangLexer.tokens, Source.fromFile(source).reader()) match {
          case BarlangParser.BarlangLexer.Success(tokens, _) =>
            BarlangParser.BarlangParser(tokens) match {
              case BarlangParser.BarlangParser.Success(sourceScript, _) =>
                val optimizedScript = Optimizer.optimize(sourceScript)

                Compiler.compileToString(optimizedScript) match {
                  case Left(error) =>
                    println(s"Compilation failed: $error")
                  case Right(result) =>
                    target match {
                      case Some(targetFile) =>
                        Files.write(targetFile.toPath, result.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)

                      case None =>
                        println(result)
                    }
                }

              case BarlangParser.BarlangParser.NoSuccess(msg, _) =>
                println(s"Failed to parse source: $msg")
            }
          case BarlangParser.BarlangLexer.NoSuccess(msg, _) =>
            println(s"Failed to tokenize source: $msg")

        }
    }
  }
}
