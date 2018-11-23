# scaladiagram

--------
Scaladiagram is a util for prasing scala file and creating UML diargram, and then saving as dot files or image.

It a tool for describling source code UML struct, and helping to understand the code.

Its delevopment processes as the following :

1. **Done** Parsing source code:  Based on [scalariform](http://scala-ide.github.com/scalariform/) jar for sketching the original file.
   Some kinds of types like **case class** /**object** /**class** /**trait** **function** etc being parsed for the following draw graph.
  
2. **Done** construct basic template for descibing UML.  

3. **Done** free software Graphviz being chosen to draw UML graph. APIs **Graphviz** developed for that.

4. **Done** combination developed code to graph.

5. **Done** debug finished. uml graph being draw, when specified the path of scala file.

6. **_Doing_** optimize code arch for being easily used, and understood. next, provied API and support cli prompt

   

