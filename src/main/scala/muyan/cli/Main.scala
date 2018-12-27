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
  /**
    * @define scan dir according command line para
    * @param para  para parsed by command line
    * */
  def scanDirectory(para: CliConfig): Unit = {
   val file = new File(para.src)
    buildFile(file, para.dst, para.format)
  }

  //build all
  def buildFile(file: File, dst: Option[String], format: String): Unit = {
    val fileList = file.listFiles()

    for (in <- fileList) {
      println(in.isFile + " -> " + in.getAbsolutePath)
      if(in.isFile ) {
        if (in.getAbsolutePath.toLowerCase.endsWith(".scala")) {
          new Builder(in.getAbsolutePath, dst, format).build
        }
      }else {
        buildFile(in, dst, format)
      }
    }
  }
  //entrance of cli command
  def main(args: Array[String]): Unit = {
    var conf = CliConfig("") //default src path is empty
    println(args.toList.mkString(" "))
    if (args.isEmpty) help()
    else {
      val itor = args.toIterator
      while (itor.hasNext){
        val i = itor.next()
        i match {
          case "-h" | "--help" => help()
          case "-src" => conf =  conf.copy(src = itor.next())
          case "-dst" => conf =  conf.copy(dst = Some(itor.next()))
          case "-f" => conf =  conf.copy(format = if(itor.hasNext) itor.next() else "pdf")
        }
      }
    }
    println("parse param finished.")
    println("begin to scan scala file.")
    scanDirectory(conf)
    println("parsing scala file finished.")
  }

  //usage command
  def help(): Unit  ={
    println("scaladiagram help list:")
    println("usage: java -jar jarName <source path> [dest path] [format]" )
    println(s" -src           scala file path(required)")
    println(s" -dst           diagram file storage path")
    println(s" -f             diagram file storage format,")
    println(s"                png,pdf,jpg define,default: pdf ")
    println(s" -h,--help      show help")

    System.exit(1)
  }
}
