package ldfi.akka

import java.io.PrintWriter
import java.io.File
import java.nio.file._
import evaluation.Evaluator

import sys.process._

import java.lang.reflect.Method

object Main {

  val basePath : String = System.getProperty("user.dir") + "/ldfi-akka/src/main/scala"

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args.toList, Context())

    if(ctx.rewrite){
      //target rewrite directory
      val progDir = ctx.programsDir match {
        case Some(pd) if pd.exists() => pd
        case Some(pd) => sys.error("Dir: '" + pd.getCanonicalPath + "' does not exist")
        case None => sys.error("No program directory was given. For usage, use --help")
      }

      scalafixRewrite(progDir)
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
    //create logs.log
    new PrintWriter("logs.log") {write("");close()}

    val (mainCls, mainMeth) = reflectClass(mainClass, "main")
    val (verifyCls, verifyMeth) = reflectClass(verifyClass, verMeth)

    val program = Program(mainCls, mainMeth, verifyCls, verifyMeth)

    Evaluator.evaluate(program)

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


  def scalafixRewrite(progDir: File): Unit = {

    //TODO: Use ScalaFix API for this
    
    //copy the source files of project to above directory
    copyProgram(progDir)

    //rewrites source files according to scalafix rules
    val rewrite = "ldfi-akka/./scalafixCli --rules github:KTH/ldfi-akka/v1.0 " + basePath + " --sourceroot ." !

    /*
    TODO: The command generates an error for some reason, so we ignore it for now.
    if(rewrite != 0)
      sys.error("Errorcode: " + rewrite + "Failed to rewrite directory: " + progDir.getCanonicalPath)
    */

  }

  def copyProgram(progDir: File): Unit = {

    val relativePath = progDir.getCanonicalPath.split(basePath).lift(1) match {
      case Some(path) => path + "/"
      case None => "/"
    }

    for (file <- progDir.listFiles()){
      if(file.isDirectory) {
        copyProgram(file)
      }
      else {
        val dest = new File(basePath + relativePath + file.getName)
        Files.copy(file.toPath, dest.toPath, StandardCopyOption.REPLACE_EXISTING)
      }
    }
  }


  def processOptions(args: List[String], ctx: Context): Context = args match {
    case "--help" :: rest =>
      displayHelp()
      sys.exit(0)

    case "--rewrite" :: rest =>
      processOptions(rest, ctx.copy(rewrite = true))

    case "-d" :: prDir :: rest =>
      processOptions(rest, ctx.copy(programsDir = Some(new File(prDir))))

    case "-m" :: mClass :: rest =>
      processOptions(rest, ctx.copy(mainClass = Some(new File(mClass))))

    case "-v" :: verClass :: verMeth :: rest =>
      processOptions(rest, ctx.copy(verifyClass = Some(new File(verClass)), verifyMethod = verMeth))

    case List() => ctx

    case unknown => sys.error("Option '" + unknown + "' was not recognized. For usage, use --help.")

  }

  def displayHelp(): Unit = {
    println("Usage: ldfi-akka [options] -d <progdir> -m <main class> -v <verify class>")
    println(" --rewrite               rewrite program using scalafix")
    println(" -d <progDir>            target directory for scalafix rewrites")
    println(" -m <myproject/main>     path to main file ")
    println(" -v <myproject/verifyclass.scala verifymethod>   path to file that returns boolean")
    println("Options include:")
    println(" --help                  displays this help")
  }

  case class Context(rewrite: Boolean = false,
                     programsDir: Option[File] = None,
                     mainClass: Option[File] = None,
                     verifyClass: Option[File] = None,
                     verifyMethod: String = null)


  case class Program(mainClass: Class[_],
                     mainMethod: Method,
                     verifyClass: Class[_],
                     verifyMethod: Method)

}


