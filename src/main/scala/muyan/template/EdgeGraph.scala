package muyan.template

/**
  * define scala object relation ship in each other
  * some of edge may not be used in UML diagram
  * */
object  EdgeGraph {
  val inherit = s"""[arrowhead="empty"]""" //relation parent child is-a
  val dependency = s"""[arrowhead = "open",style = "dashed]""" // a using relationship
  val realization = s"""[arrowhead = "emtpy",style = "dashed]""" //
  val composition = s"""[arrowhead = "diamond"]""" //integral part contain-a
  val aggregation = s"""[arrowhead = "odiamond"]""" //has-a
  val association = s"""[arrowhead = "open"]"""
}
