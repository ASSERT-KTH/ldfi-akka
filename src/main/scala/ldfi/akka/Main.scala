package ldfi.akka

import java.io.PrintWriter
import java.io.File
import java.nio.file._

import sys.process._

object Main {

  val basePath : String = System.getProperty("user.dir") + "/ldfi-akka/program"


  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args.toList, Context())

    val progDir = ctx.programsDir match {
      case Some(pd) if pd.exists() => pd
      case Some(pd) => sys.error("Dir: '" + pd.getCanonicalPath + "' does not exist")
      case None => sys.error("No program directory was given. For usage, use --help")
    }

    scalafixRewrite(progDir)

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

    new PrintWriter("logs.log") {write("");close()}
    //Evaluator.evaluate("SimpleDeliv")


  }


  def scalafixRewrite(progDir: File): Unit = {
    copyProgram(progDir)

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

    case "-d" :: progDir :: rest =>
      processOptions(rest, ctx.copy(programsDir = Some(new File(progDir))))

    case "-m" :: mainClass :: rest =>
      processOptions(rest, ctx.copy(mainClass = Some(new File(mainClass))))

    case "-v" :: verifyClass :: rest =>
      processOptions(rest, ctx.copy(verifyClass = Some(new File(verifyClass))))

    case List() => ctx

    case unknown => sys.error("Option '" + unknown + "' was not recognized. For usage, use --help.")

  }

  def displayHelp(): Unit = {
    println("Usage: ldfi-akka [options] -d <progdir> -m <main class> -v <verify class>")
    println(" -d <progDir>            target directory for scalafix rewrites")
    println(" -m <myproject/main>     path to main file ")
    println(" -v <myproject/verify>   path to file that returns boolean")
    println("Options include:")
    println(" --help                  displays this help")
  }

  case class Context(programsDir: Option[File] = None,
                     mainClass: Option[File] = None,
                     verifyClass: Option[File] = None)
}


