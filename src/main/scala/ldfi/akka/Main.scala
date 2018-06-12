package ldfi.akka

import java.io.PrintWriter
import java.io.File
import java.nio.file._
import sys.process._

import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.lang.reflect.Type
import java.util.Locale

object Main {

  val basePath : String = System.getProperty("user.dir") + "/ldfi-akka/program"

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args.toList, Context())

    if(ctx.rewrite){
      //scalafixRewrite(progDir)
      scalafixRewrite()
    }

    else{

      /*
      val progDir = ctx.programsDir match {
        case Some(pd) if pd.exists() => pd
        case Some(pd) => sys.error("Dir: '" + pd.getCanonicalPath + "' does not exist")
        case None => sys.error("No program directory was given. For usage, use --help")
      }
      */

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

    val (mainCls, mainClsInst, mainMeth) = getMain(mainClass)
    val (verifyCls, verifyInstCls, verifyMeth) = getVerify(verifyClass, verMeth)

    val program = Program(mainCls, mainClsInst, mainMeth, verifyCls, verifyInstCls, verifyMeth)

    Evaluator.evaluate(program)

  }

  def getMain(mainClass: File): (Class[_], Any, Method) = {
    try {
      val mainCls = Class.forName(mainClass.getName)
      val mainClsInst = mainCls.newInstance()

      val methods : List[Method] = mainCls.getDeclaredMethods.toList
      val mainMethod = methods.find(p => p.getName == "main") match {
        case Some(mainMeth) => mainMeth
        case None => sys.error("Reflection error: could not find main method in main class")
      }
      (mainCls, mainClsInst, mainMethod)
    }
    catch {
      case cnfe: ClassNotFoundException =>
        cnfe.printStackTrace()
        sys.error("Reflection error, ClassNotFoundException for Class: " + mainClass);
      case inse: InstantiationException =>
        inse.printStackTrace()
        sys.error("Reflection error, InstantiationException for Class: " + mainClass);
      case iae: IllegalAccessException =>
        iae.printStackTrace()
        sys.error("Reflection error, IllegalAccessException for Class: " + mainClass);
    }
  }


  def getVerify(verifyClass: File, verMeth: String): (Class[_], Any, Method) = {
    try {
      val verifyCls = Class.forName(verifyClass.getName)
      val verifyClsInst = verifyCls.newInstance()

      val methods : List[Method] = verifyCls.getDeclaredMethods.toList
      val verifyMethod = methods.find(p => p.getName == verMeth) match {
        case Some(vMeth) => vMeth
        case None => sys.error("Reflection error: could not find verify method in verify class")
      }
      (verifyCls, verifyClsInst, verifyMethod)
    }
    catch {
      case cnfe: ClassNotFoundException =>
        cnfe.printStackTrace();
        sys.error("Reflection error, ClassNotFoundException for Class: " + verifyClass);
      case inse: InstantiationException =>
        inse.printStackTrace()
        sys.error("Reflection error, InstantiationException for Class: " + verifyClass);
      case iae: IllegalAccessException =>
        iae.printStackTrace()
        sys.error("Reflection error, IllegalAccessException for Class: " + verifyClass);
    }
  }



  def scalafixRewrite(): Unit = {
    //TODO: Use scalafixcli API for this

    val rewrite = "./scalafixCli --rules github:KTH/ldfi-akka/v1.0 --sourceroot ." !
  }


  def scalafixRewrite(progDir: File): Unit = {
    //creating Program directory in ldfi-akka/program
    val path = Paths.get(basePath)
    Files.createDirectories(path)

    //copy the source files of project to above directory
    copyProgram(progDir)

    //rewrites source files according to scalafix rules
    val rewrite = "./scalafixCli --rules github:KTH/ldfi-akka/v1.0 " + basePath + " --sourceroot ." !

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
    println(" -v <myproject/verify>   path to file that returns boolean")
    println("Options include:")
    println(" --help                  displays this help")
  }

  case class Context(rewrite: Boolean = false,
                     programsDir: Option[File] = None,
                     mainClass: Option[File] = None,
                     verifyClass: Option[File] = None,
                     verifyMethod: String = null)


  case class Program(mainClass: Class[_],
                     mainClassInstance: Any,
                     mainMethod: Method,
                     verifyClass: Class[_],
                     verifyClassInstance: Any,
                     verifyMethod: Method)
}


