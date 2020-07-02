package one.lab.tasks.week.two

import java.nio.file.Files
import java.nio.file.Paths
import scala.io.StdIn.readLine

import scala.jdk.CollectionConverters._
import scala.util.chaining._

/**
  * Можете реализовать свою логиprintln("Checking List(1,2,3,4,5)")ку.
  * Главное чтобы работали команды и выводились ошибки при ошибочных действиях.
  * ll - показать все что есть в тек. папке
  * dir - показать только директории в тек. папке
  * ls - показать только файлы в тек. папке
  * cd some_folder - перейте из тек. папки в другую (учитывайте что путь можно сделать самым простым
  * то есть если я сейчас в папке /main и внутри main есть папка scala и я вызову
  * cd scala то мы должны просто перейти в папку scala. Реализация cd из текущей папки
  * по другому ПУТИ не требуется. Не забудьте только реализовать `cd ..`)
  *
  * Бонусные команды и идеи привествуются.
  */
object FileManager extends App {

  trait Command {
    def isSubstitutive: Boolean = false
  }

  case class PrintErrorCommand(val error: String) extends Command

  case class ListDirectoryCommand() extends Command

  case class ListFilesCommand() extends Command

  case class ListAllContentCommand() extends Command

  case class ChangeDirectoryCommand(val destination: String) extends Command {
    override val isSubstitutive: Boolean = true
  }

  case class ChangePathError(error: String)

  case class Path(var path: String)

  val currPath = Path("/usr")

  def getFilesDirs(path: String): List[String] = {
    Files.list(Paths.get(path)).iterator().asScala.map(path => path.toFile.getName).map(name => s"$path/$name").toList
  }

  def getFiles(path: String): List[String] = {
    Files.list(Paths.get(path)).iterator().asScala.filter(path => path.toFile.isFile).map(path => path.toFile.getName).map(name => s"$path/$name").toList
  }

  def getDirectories(path: String): List[String] = {
    Files.list(Paths.get(path)).iterator().asScala.filter(path => path.toFile.isDirectory).map(path => path.toFile.getName).map(name => s"$path/$name").toList
  }

  def changePath(current: String, path: String): Either[ChangePathError, String] = {
    if (path.equals("..")) {
      val currSplit = current.split("/")
      val newPath = current.substring(0, (current.length() - currSplit(currSplit.length - 1).length - 1))
      currPath.path = newPath
      Right(s"$newPath")
    }
    else {
      val dirs = getDirectories(current)
      if (dirs.contains(s"$current/$path")) {
        currPath.path = s"$current/$path"
        Right(s"$current/$path")
      }
      else {
        Left(new ChangePathError("path does not exist!"))
      }
    }
  }

  def parseCommand(input: String): Command = input match {
    case "ll" => ListAllContentCommand()
    case "ls" => ListFilesCommand()
    case "dir" => ListDirectoryCommand()
    case str if str.startsWith("cd") => {
      val split = str.split(" ")
      if (split.length == 1) {
        PrintErrorCommand("wrong command")
      } else {
        ChangeDirectoryCommand(split(1))
      }
    }
    case _ => PrintErrorCommand("wrong command!")
  }


  def handleCommand(command: Command, currentPath: String): String = command match {
    case ListFilesCommand() => getFiles(currentPath).mkString("\n")
    case ListDirectoryCommand() => getDirectories(currentPath).mkString("\n")
    case ListAllContentCommand() => getFilesDirs(currentPath).mkString("\n")
    case ChangeDirectoryCommand(destination) => {
      val newPath = changePath(currentPath, destination).getOrElse(changePath(currentPath, destination).left.get.error)
      newPath
    }
    case PrintErrorCommand(error) => error
  }

  def main(basePath: String): Unit = {
    println(s"You are in directory -> $basePath")
    while (true) {
      val curr = currPath.path
      val command = readLine()
      println(handleCommand(parseCommand(command), curr))
    }
  }

  main(currPath.path)
}
