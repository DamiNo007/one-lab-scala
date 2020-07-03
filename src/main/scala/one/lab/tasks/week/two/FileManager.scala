package one.lab.tasks.week.two

import java.nio.file.{Files, Paths, StandardOpenOption}

import scala.io.Source
import scala.io.StdIn.readLine
import scala.jdk.CollectionConverters._
import scala.util.chaining._

/**
  * Можете реализовать свою логиprintln("Checking List(1,2,3,4,5)")ку.
  * Главное чтобы работали команды и выводились ошибки при ошибочных действиях.
  * ll - показать все что есть в тек. папке
  * dir - показать только директории в тек. папке
  * ls - показать только файлы в тек. папке
  * mkdir - создать директорию
  * mkfile - создать файл
  * deldir - удалить директорию
  * delfile - удалить файл
  * outfile file.txt - вывести содержимое файла
  * wrfile file.txt text - вставить в файл текст
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

  case class MakeDir(val name: String) extends Command

  case class MakeFile(val name: String) extends Command

  case class DelDir(val name: String) extends Command

  case class DelFile(val name: String) extends Command

  case class GetFileContent(val name: String) extends Command

  case class WriteToFile(val fileName: String, val text: String) extends Command

  case class ChangeDirectoryCommand(val destination: String) extends Command {
    override val isSubstitutive: Boolean = true
  }

  case class FileExistsError(val error: String)

  case class FileDoesNotExistError(val error: String)

  case class DirectoryExistsError(val error: String)

  case class DirectoryDoesNotExistError(val error: String)

  case class ChangePathError(val error: String)

  case class Path(var path: String)

  val currPath = Path("/home/damir/IdeaProjects/one-lab-scala1")

  def getFilesDirs(path: String): List[String] = {
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .map(path => path.toFile.getName)
      .map(name => s"$path/$name")
      .toList
  }

  def getFiles(path: String): List[String] = {
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .filter(path => path.toFile.isFile)
      .map(path => path.toFile.getName)
      .map(name => s"$path/$name")
      .toList
  }

  def getDirectories(path: String): List[String] = {
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .filter(path => path.toFile.isDirectory)
      .map(path => path.toFile.getName)
      .map(name => s"$path/$name")
      .toList
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
        Left(ChangePathError("path does not exist!"))
      }
    }
  }

  def mkFile(fileName: String, currentPath: String): Either[FileExistsError, String] = {
    val files = getFiles(currentPath)
    if (!files.contains(s"$currentPath/$fileName")) {
      val path = Paths.get(s"$currentPath/$fileName")
      Files.createFile(path)
      Right(s"file $fileName was created")
    } else {
      Left(FileExistsError("file with this name already exists!"))
    }
  }

  def mkDir(dirName: String, currentPath: String): Either[DirectoryExistsError, String] = {
    val dirs = getDirectories(currentPath)
    if (!dirs.contains(s"$currentPath/$dirName")) {
      val path = Paths.get(s"$currentPath/$dirName")
      Files.createDirectory(path)
      Right(s"directory $dirName was created")
    } else {
      Left(DirectoryExistsError("directory with this name already exists!"))
    }
  }

  def removeFile(fileName: String, currentPath: String): Either[FileDoesNotExistError, String] = {
    val path = Paths.get(s"$currentPath/$fileName")
    if (Files.deleteIfExists(path))
      Right(s"file $fileName was deleted")
    else
      Left(FileDoesNotExistError("this file does not exist!"))
  }

  def removeDir(dirName: String, currentPath: String): Either[DirectoryDoesNotExistError, String] = {
    val path = Paths.get(s"$currentPath/$dirName")
    if (Files.deleteIfExists(path))
      Right(s"directory $dirName was deleted")
    else
      Left(DirectoryDoesNotExistError("this directory does not exist!"))
  }

  def getFileContent(fileName: String, currentPath: String): Either[FileDoesNotExistError, String] = {
    val files = getFiles(currentPath)
    if (files.contains(s"$currentPath/$fileName")) {
      val src = Source.fromFile(s"$currentPath/$fileName")
      try
        Right(src.getLines().mkString("\n"))
      finally
        src.close()
    } else {
      Left(FileDoesNotExistError("this file does not exist!"))
    }
  }

  def writeToFile(fileName: String, currentPath: String, text: String): Either[FileDoesNotExistError, String] = {
    val files = getFiles(currentPath)
    if (files.contains(s"$currentPath/$fileName")) {
      val path = Paths.get(s"$currentPath/$fileName")
      Files.write(path, text.getBytes, StandardOpenOption.APPEND)
      Right(s"appended text $text to file")
    } else {
      Left(FileDoesNotExistError("this file does not exist!"))
    }
  }

  def parseCommand(input: String): Command = input.trim match {
    case "ll" => ListAllContentCommand()
    case "ls" => ListFilesCommand()
    case "dir" => ListDirectoryCommand()
    case str if str.startsWith("mkfile") => {
      val split = str.split(" ")
      if (split.length == 1)
        PrintErrorCommand("wrong command")
      else
        MakeFile(split(1))
    }
    case str if str.startsWith("mkdir") => {
      val split = str.split(" ")
      if (split.length == 1)
        PrintErrorCommand("wrong command")
      else
        MakeDir(split(1))
    }
    case str if str.startsWith("delfile") => {
      val split = str.split(" ")
      if (split.length == 1)
        PrintErrorCommand("wrong command")
      else
        DelFile(split(1))
    }
    case str if str.startsWith("deldir") => {
      val split = str.split(" ")
      if (split.length == 1)
        PrintErrorCommand("wrong command")
      else
        DelDir(split(1))
    }
    case str if str.startsWith("outfile") => {
      val split = str.split(" ")
      if (split.length == 1)
        PrintErrorCommand("wrong command")
      else
        GetFileContent(split(1))
    }
    case str if str.startsWith("wrfile") => {
      val split = str.split(" ")
      if (split.length == 1 || split.length == 2)
        PrintErrorCommand("wrong command")
      else {
        WriteToFile(split(1), str.substring(split(0).length + split(1).length + 2, str.length) + "\n")
      }
    }
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
    case MakeFile(name) => mkFile(name, currentPath).getOrElse(mkFile(name, currentPath).left.get.error)
    case MakeDir(name) => mkDir(name, currentPath).getOrElse(mkDir(name, currentPath).left.get.error)
    case DelFile(name) => removeFile(name, currentPath).getOrElse(removeFile(name, currentPath).left.get.error)
    case DelDir(name) => removeDir(name, currentPath).getOrElse(removeDir(name, currentPath).left.get.error)
    case GetFileContent(name) => getFileContent(name, currentPath).getOrElse(getFileContent(name, currentPath).left.get.error)
    case WriteToFile(name, text) => writeToFile(name, currentPath, text).getOrElse(writeToFile(name, currentPath, text).left.get.error)
    case ChangeDirectoryCommand(destination) => changePath(currentPath, destination).getOrElse(changePath(currentPath, destination).left.get.error)
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
