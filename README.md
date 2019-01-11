# scaladiagram

scaladiagram is a tool for parsing Scala file and saving UML diagram to kinds of files.

the tool for describing source code UML struction, and helping to understanding Scala code.


# dependcy

Program depends on [graphviz](https://graphviz.gitlab.io/). Before running the program, graphviz must bu install on OS.



# config 

see the detail resource file like the following:

> ```
> tmpDir= c:/temp  
> graphDir =C:\\Program Files (x86)\\Graphviz2.38\\bin\\dot.exe //graphviz install directory
> ```



# package

> sbt assembly



# how to use it

- help

> java -jar scaladiagram-assembly-0.1.jar -h  //output help

- generate uml files

> java -jar scaladiagram-assembly-0.1.jar -src <source path> \[dest path] [format]
