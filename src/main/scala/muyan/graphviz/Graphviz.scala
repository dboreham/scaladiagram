package muyan.graphviz

import java.io.{File, FileOutputStream}
import java.util.ResourceBundle

import muyan.diagramsketch._
import muyan.template.DigraphBase
import muyan.template.EdgeGraph.{composition, inherit}
import scala.language.implicitConversions

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
  private  lazy  val conf = ResourceBundle.getBundle("config")
  val tmpDir  = Try{conf.getString("tmpDir")}.getOrElse("/") //set default path /
  val dstPath = dst.getOrElse(tmpDir)
  val graphExecutable  =  Try{conf.getString("graphDir")}.getOrElse("/")

  /**
  * write dot string @param src src to @see tmpDir and return file handle
  * @param src dot stream
  * @param file specified the file name
  * */
  def writeDotToFile(src: String, file: File) = {
    val fop = new FileOutputStream(file)
    try{
      if (!file.exists) {

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

    val args = s"$graphExecutable -T $format $path -o $dest.$format"
    val runTime = Runtime.getRuntime
    println(args)
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

  def draw(fileName: String) = {
    val ctx = graphContent
    val src = s"$tmpDir/$fileName.dot.tmp"
    writeDotToFile(ctx ,new File(src))
    buildGraph(src ,s"$dstPath/$fileName")
//    deleteTmpFile(new File(src))

  }

  def deleteTmpFile(file: File): Unit = {
    if(file.isFile) file.delete()
  }
 
  def buildUml(name: String) = {
    draw(name)
    clear
  }

  implicit def sketchToSting(t: List[String]) :String = t.mkString

  //dot builder for parse
  def dotBuilder(buff: List[DiagramLite]) =  {
    buff.foreach{
      case DiagramLite(clz, ext, inner, method, attr, pack, _) =>
        val filterClz = buff.filter{ el => el.clz.alias.equals(clz.alias)}
        val isExist = filterClz.forall{x => isExistElement(x.inner, x.method, x.attr, x.pack)}

        if (isExist || (filterClz.length > 1 && isExistElement(inner, method, attr, pack))) {
          addItem(clz.alias, method.flatMap( e => e.descSketch), attr.flatMap( e => e.descSketch), clz.descSketch)
        }

        val extCtx: List[String] = if(ext.isDefined) ext.get.descSketch else Nil
        //todo fix class has same name
        //        (extCtx.filterNot(clz.descSketch.contains(_))).foreach {
        //          name => addItem(name, Nil,Nil)
        //        }

        extCtx.foreach {
          o => addRelation(clz.alias, o, inherit)
        }

        inner.foreach {el =>
          addRelation(clz.alias, el.descSketch, composition)}
    }

  }

  private def isExistElement(inner: List[ClazzSketch],
                             method: List[FunctionSketch],
                             attr: List[AttributeSketch],
                             pack: PackageSketch) : Boolean = {
    // in include element
    if (attr.nonEmpty || inner.nonEmpty || method.nonEmpty || pack.name.nonEmpty) true
    else false
  }

} 
