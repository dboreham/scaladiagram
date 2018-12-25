package muyan.cli

import java.io.File
import muyan.Builder

case class CliConfig(
                    src: String,
                    dst: Option[String] = None,
                    format: String = "PDF"
                    )

object Main {

  //todo multi thread create graph
  def scanDirectory(path: String): Unit = {
   val file = new File(path)
    buildFile(file)
  }

  //build all
  def buildFile(file: File): Unit = {
    val fileList = file.listFiles()
    println("file list: " + fileList.size)
    for (in <- fileList) {
      println(in.isFile + " -> " + in.getAbsolutePath)
      if(in.isFile ) {
        if (in.getAbsolutePath.toLowerCase.endsWith(".scala")) {
          new Builder(in.getAbsolutePath).build
        }
      }else {
        buildFile(in)
      }
    }
  }
  //entrance of cli command
  def main(args: Array[String]): Unit = {
    var conf = CliConfig("") //default src path is empty
    if (args.isEmpty) help()
    else {
      val itor = args.toIterator
      while (itor.hasNext){
        val i = itor.next()
        println("print arg: "+ i)
        i match {
          case "-h" | "--help" => help()
          case "-src" => conf =  conf.copy(src = itor.next())
          case "-dst" => conf =  conf.copy(dst = Some(itor.next()))
          case "-format" => conf =  conf.copy(format = itor.next())
        }
      }
    }
    println("parse param finished.")
    println("begin to scan scala file for diagram.")
    scanDirectory(conf.src)

  }

  //usage command
  def help(): Unit  ={
    println("scaladiagram help list:" )
    println("usage: java -jar jarName <source path> [dest path] [format]" )
    println(s" -src           scala file path(required)")
    println(s" -dst           diagram file storage path")
    println(s" -format        diagram file storage format(default:pdf) ")
    println(s" -h,--help      show help")

    System.exit(1)
  }
}
