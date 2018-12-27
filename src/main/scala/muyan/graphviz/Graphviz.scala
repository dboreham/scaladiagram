package muyan.graphviz

import java.io.{File, FileOutputStream}
import java.util.ResourceBundle

import muyan.template.DigraphBase

import scala.util.Try

/**
  * An APIs for creating DOT file and saving formatted file such as jpg pdf.
  * All of that based on open source software GRAPHVIZ
  *
  * @see http://www.graphviz.org
  * anyone can distribute and copy it.
  * */
class Graphviz(dst: Option[String], format: String) extends DigraphBase{
//get config dir from file
  private val conf = ResourceBundle.getBundle("config")
  val tmpDir  = Try{conf.getString("tmpDir")}.getOrElse("/") //set default path /
  val dstPath = dst.getOrElse(tmpDir)
  val graphExecutable  =  Try{conf.getString("graphDir")}.getOrElse("/")

  /**
  * write dot string @param src src to @see tmpDir and return file handle
  * @param src dot stream
  * @param fileName specified the file name
  * */
  def writeDotToFile(src: String, fileName: String) = {
    val file = new File(s"$tmpDir/$fileName.dot.tmp")
    val fop = new FileOutputStream(file)
    try{
      if (!file.exists) {
        println("file not exist")
        file.createNewFile
      }
      fop.write(src.getBytes)
      fop.flush
    } catch {
      case  ex : Exception => println(s"write file failure ${ex.printStackTrace()}")
    }finally {
      if(fop != null) fop.close()
    }
  }

  /**
    * call graphviz command line to build graph file, and save in specified directory
    * @param path file absolute path
    * @param dest destination path with file name
    * */
  def buildGraph(path: String, dest: String) ={

    val args = s"$graphExecutable -T $format:cairo:gd $path -o $dest.$format"
    val runTime = Runtime.getRuntime

    try {
      runTime.exec(args)
    }
    catch {
      case e: Exception => e.printStackTrace()
    }

/*    val f = Future {
      runTime.exec(args)
    }
    Await.result(f, 3 seconds )
    f.onComplete({
      case Failure(e)=> e.printStackTrace()
      case _ =>
    })*/

  }

  def draw(fileName: String, dest: String = tmpDir) = {
    val ctx = graphContent
    writeDotToFile(ctx ,fileName)
    val src = s"$tmpDir/$fileName.dot.tmp"
    buildGraph(src ,s"$dstPath/$fileName")

  }

  def deleteTmpFile(file: File): Unit = {
    if(file.isFile) file.delete()
  }

} 
