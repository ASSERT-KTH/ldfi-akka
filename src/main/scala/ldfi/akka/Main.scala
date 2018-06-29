package ldfi.akka

import java.io.PrintWriter
import java.io.File
import java.nio.file._

import evaluation.Evaluator

import sys.process._
import java.lang.reflect.Method

import org.apache.commons.io.FileUtils

import scala.io.Source

object Main {

  val basePath : String = System.getProperty("user.dir") + "/ldfi-akka/src/main/scala"

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args.toList, Context())

    if(ctx.cpy){
      val progDir = ctx.programsDir match {
        case Some(pd) if pd.exists() => pd
        case Some(pd) => sys.error("Dir: '" + pd.getCanonicalPath + "' does not exist")
        case None => sys.error("No program directory was given. For usage, use --help")
      }

      //copy the source files of project to above directory
      copyProgram(progDir, progDir)
    }
    else if(ctx.rewrite){
      //target rewrite directory
      scalafixRewrite()
    }
    else{

      val mainClass = ctx.mainClass match {
        case Some(mc) if mc.exists() => mc
        case Some(mc) => sys.error("Main Class: '" + mc.getCanonicalPath + "' does not exist")
        case None => sys.error("No main class was given. For usage, use --help")
      }

      val verifyClass = ctx.verifyClass match {
        case Some(vc) if vc.exists() => vc
        case Some(vc) => sys.error("Verify Class: '" + vc.getCanonicalPath + "' does not exist")
        case None => sys.error("No verify class was given. For usage, use --help")
      }

      val verifyMethod = ctx.verifyMethod match {
        case null => sys.error("No verify class method was given. For usage, use --help")
        case verMeth => verMeth
      }

      startEval(mainClass, verifyClass, verifyMethod)
    }

  }

  def startEval(mainClass: File, verifyClass: File, verMeth: String): Unit = {
    //create log file
    val log = new File("ldfi-akka/logs.log")
    log.createNewFile()

    //clear it
    new PrintWriter("ldfi-akka/logs.log") {
      write("")
      close()
    }

    val freePassMessages = try {
      Source.fromFile("freePassMessages.txt").getLines().toList
    }
    catch {
      case e: Exception => sys.error("Exception when reading from freePassMessages.txt. " +
        "If there are no free pass messages, create an empty freePassMessages.txt in root directory.")
    }

    val (mainCls, mainMeth) = reflectClass(mainClass, "main")
    val (verifyCls, verifyMeth) = reflectClass(verifyClass, verMeth)

    val program = Program(mainCls, mainMeth, verifyCls, verifyMeth)

    Evaluator.evaluate(program, freePassMessages)

  }

  def reflectClass(cls: File, methodName: String): (Class[_], Method) = {
    try {
      //convert path to specified class: e.g. com/proj/Main.scala => com.proj.Main
      val relativePath = cls.getCanonicalPath.split("src/main/scala/").lift(1) match {
        case Some(path) => path.replaceAll("/", ".").replaceAll(".scala", "")
        case None => cls.getName
      }

      val clazz = Class.forName(relativePath)
      val methods : List[Method] = clazz.getDeclaredMethods.toList
      val methodInst = methods.find(p => p.getName == methodName) match {
        case Some(meth) => meth
        case None => sys.error("Reflection error: could not find method: " + methodName + " in class: " + cls.getName)
      }
      (clazz, methodInst)
    }
    catch {
      case cnfe: ClassNotFoundException =>
        cnfe.printStackTrace()
        sys.error("Reflection error, ClassNotFoundException for Class: " + cls)
      case inse: InstantiationException =>
        inse.printStackTrace()
        sys.error("Reflection error, InstantiationException for Class: " + cls)
      case iae: IllegalAccessException =>
        iae.printStackTrace()
        sys.error("Reflection error, IllegalAccessException for Class: " + cls)
    }
  }


  def scalafixRewrite(): Unit = {

    //TODO: Use ScalaFix API for this

    //rewrites source files according to scalafix rules
    val rewrite = "ldfi-akka/./scalafixCli --rules github:KTH/ldfi-akka/v1.0 " + basePath + " --sourceroot ldfi-akka" !

    /*
    TODO: The command generates an error for some reason, so we ignore it for now.
    if(rewrite != 0)
      sys.error("Errorcode: " + rewrite + "Failed to rewrite directory: " + progDir.getCanonicalPath)
    */

  }

  def copyProgram(mainDir: File, subDir: File): Unit = {

    for (file <- subDir.listFiles()){

      val relativePath = file.getPath.split(mainDir.toString).lift(1) match {
        case Some(path) => path + "/"
        case None => "/"
      }

      val destPath = basePath + relativePath
      val dest = new File(destPath)

      if(file.isDirectory) {
        val f = new File(destPath)
        if(f.exists()) {
          FileUtils.deleteDirectory(f)
        }
        Files.copy(file.toPath, dest.toPath, StandardCopyOption.REPLACE_EXISTING)
        copyProgram(mainDir, file)
      }
      else{
        Files.copy(file.toPath, dest.toPath, StandardCopyOption.REPLACE_EXISTING)
      }
    }

  }

  def processOptions(args: List[String], ctx: Context): Context = args match {
    case "--help" :: rest =>
      displayHelp()
      sys.exit(0)

    case "--copy" :: prDir :: rest =>
      processOptions(rest, ctx.copy(cpy = true, programsDir = Some(new File(prDir))))

    case "--rewrite" :: rest =>
      processOptions(rest, ctx.copy(rewrite = true))

    case "-m" :: mClass :: rest =>
      processOptions(rest, ctx.copy(mainClass = Some(new File(mClass))))

    case "-v" :: verClass :: verMeth :: rest =>
      processOptions(rest, ctx.copy(verifyClass = Some(new File(verClass)), verifyMethod = verMeth))

    case List() => ctx

    case unknown => sys.error("Option '" + unknown + "' was not recognized. For usage, use --help.")

  }

  def displayHelp(): Unit = {
    println("Usage: ldfi-akka [options] -d <progdir> -m <main class> -v <verify class>")

    println(" --copy <myproject/>                             copy program in to ldfi-akka src dir")
    println(" --rewrite                                       rewrite program using scalafix")
    println(" -d <progDir>                                    target directory for scalafix rewrites")
    println(" -m <myproject/main>                             path to main file ")
    println(" -v <myproject/verifyclass.scala verifymethod>   path to file that returns boolean")

    println("Options include:")
    println(" --help                  displays this help")
  }

  case class Context(cpy: Boolean = false,
                     programsDir: Option[File] = None,
                     rewrite: Boolean = false,
                     mainClass: Option[File] = None,
                     verifyClass: Option[File] = None,
                     verifyMethod: String = null)


  case class Program(mainClass: Class[_],
                     mainMethod: Method,
                     verifyClass: Class[_],
                     verifyMethod: Method)

}


