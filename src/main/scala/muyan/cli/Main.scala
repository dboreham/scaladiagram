package muyan.cli

import java.io.File
import muyan.diagramsketch.{DiagramLite, DiagramSketch}
import muyan.graphviz.Graphviz

case class CliConfig(
                    src: String,
                    dst: Option[String] = None,
                    format: String = "png",
                    arrange: String = "package"
                    )

object Main extends DiagramSketch{
 //cache context for drawing uml by tool of graphviz
  var cache: List[DiagramLite] = Nil
  /**
    * @define scan dir according command line para
    * @param para  para parsed by command line
    * */
  def scanDirectory(para: CliConfig): Unit = {
   val file = new File(para.src)
   buildFile(file, para.dst, para.format)
   val builder = new Graphviz(para.dst,para.format)

    try {
      if(para.arrange.equalsIgnoreCase("file")) {
        cache.foreach {
          el =>
            builder.dotBuilder(List(el))
            builder.buildUml(el.fileName)
        }
      } else {//all classes in package in one file
        for (el <- cache.groupBy{el => el.pack.name}) {
          el._2.foreach{ el => builder.dotBuilder(List(el))}
          builder.buildUml(el._1)
        }
      }
    }catch {
      case ex: Exception => println(ex.getMessage)
    }
  }

  //build all
  def buildFile(file: File, dst: Option[String], format: String): Unit = {
    val fileList: Array[File] = if(file.isFile) Array[File]{file} else file.listFiles()
    for (in <- fileList) {
      println(in.isFile + " -> " + in.getAbsolutePath)
      if(in.isFile ) {
        if (in.getAbsolutePath.toLowerCase.endsWith(".scala")) {
         try {
          cache = catalystSketch(in.getAbsolutePath).toList ::: cache
         }catch {
           case ex: Exception => println(ex.getMessage)
         }
        }
      }else {
        buildFile(in, dst, format)
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
        i match {
          case "-h" | "--help" => help()
          case "-src" => conf =  conf.copy(src = itor.next())
          case "-dst" => conf =  conf.copy(dst = Some(itor.next()))
          case "-x" => conf =  conf.copy(dst = Some(itor.next()))
          case "-f" => conf =  conf.copy(arrange = if(itor.hasNext) itor.next() else conf.arrange)
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
    println("usage: java -jar jarName -src <source path> <-dst> [dest path] <-f> [format] <-x> [uml arrange]" )
    println(s" -src           scala file path(required)")
    println(s" -dst           diagram file storage path")
    println(s" -f             diagram file storage format,")
    println(s" -x             draw uml in file, package,")
    println(s"                png,pdf,jpg define,default: png ")
    println(s" -h,--help      show help")

    System.exit(1)
  }
}
