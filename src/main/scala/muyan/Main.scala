package muyan

import java.io.File

object Main {

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
}
