package muyan.graphviz

import java.io.{BufferedWriter, File, FileOutputStream, FileWriter}
import java.util.ResourceBundle

import scala.concurrent.Future
import scala.util.{Failure, Try}

/**
  * An APIs for creating DOT file and saving formatted file such as jpg pdf.
  * All of that based on open source software GRAPHVIZ
  *
  * @see http://www.graphviz.org
  * anyone can distribute and copy it.
  * */
class Graphviz extends DigraphBase {
//get config dir from file
 private lazy val conf = ResourceBundle.getBundle("config")
  val tmpDir  = Try{conf.getString("tmpDir")}.getOrElse("/") //set default path /
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
    * @param dest destination path
    * */
  def buildGraph(path: String, dest: String) ={
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future
    val outputFormat ="pdf"
    val args = s"$graphExecutable -T $outputFormat $path -o $dest/${System.currentTimeMillis()}.$outputFormat"
    val runTime = Runtime.getRuntime
    val f = Future {
      runTime.exec(args)
    }

    f.onComplete({
      case Failure(e)=> e.printStackTrace()
      case _ =>
    })
  }
 
   def draw(fileName: String, dest: String = tmpDir) = {
    val ctx = graphContent
    writeDotToFile(ctx ,fileName)
    buildGraph(s"$tmpDir/$fileName.dot.tmp",tmpDir)
  }


} 
