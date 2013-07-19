import scala.io.Source

object Solver {

  def main(args: Array[String]) : Unit = {
    println("Hello Scala")
    val graph = new Graph(args(0))
    println("nodes: " + graph.nodeCount + " edges: " + graph.edgeCount)
    solve(graph)
  }
  
  def solve(graph: Graph) : Unit = {
    val domain = new Domain(graph.nodeCount)
    var constraints = graph.edges.foldLeft(Set[Constraint]())( (z, edge) => z + new NotEqualConstraint(edge(0), edge(1)) )   
    var coloredNodes = 0;
    var color = 1;
    while(coloredNodes < graph.nodeCount) {
      //loop through all nodes
      var index = 0;
      while (index < graph.nodeCount) {
        if (domain.solution(index) == 0) {
	      //Try coloring the next node with this same color
	      domain.solution(index) = color
	      
	      var constrantsAdded = Set[Constraint]()
	      var constraintsToAdd= constraints.foldLeft(Set[Constraint]())( (z, c) => z ++ c.prune(domain, constraints) )
	      while(!constraintsToAdd.isEmpty) {
	        constrantsAdded = constrantsAdded ++ constraintsToAdd 
	        constraintsToAdd= constraints.foldLeft(Set[Constraint]())( (z, c) => z ++ c.prune(domain, constraints ++ constrantsAdded) )
	      }
	      val extendedConstraints = constraints ++ constrantsAdded
	      if (extendedConstraints.foldLeft(true)((z, c) => z && c.isValid(domain)) ) {
	        //if all constraints valid, add in the extended constraints and continue solution
	        constraints = extendedConstraints
	        coloredNodes = coloredNodes + 1;
	      } else {
	        //this solution is no good. Continue 
	        domain.solution(index) = 0
	      }
        }
	    index = index + 1
	  }
      //Increment the color, since we've colored all the nodes we can with the previous color
      color = color + 1
    }
    
    domain.solution.foreach(node => print(node + " "))
  }
  
  abstract class Constraint {
    def isValid(domain: Domain): Boolean
    def prune(domain: Domain, constraints: Set[Constraint]): Set[Constraint]
  }
  
  case class NotEqualConstraint(a: Int, b: Int) extends Constraint {
    override def isValid(domain: Domain): Boolean = {
      val colorA = domain.solution(a);
      val colorB = domain.solution(b);
      if (colorA > 0 && colorB > 0) colorA != colorB else true 
    }
    
    override def prune(domain: Domain, constraints: Set[Constraint]): Set[Constraint] = {
      val colorA = domain.solution(a);
      val colorB = domain.solution(b);
      
      var foundA = false;
      var foundB = false;
      constraints.foreach( 
          c => c match {
            case NotColorConstraint(x, color) =>
              foundA = foundA || (x == a && color == colorB)
              foundB = foundB || (x == b && color == colorA)
            case _ =>
          })
          
      val result = if (colorA > 0 && !foundB) Set[Constraint](new NotColorConstraint(b, colorA)) else Set[Constraint]()
      if (colorB > 0 && !foundA) result + new NotColorConstraint(a, colorB) else result
    }
  }
  
  case class NotColorConstraint(a: Int, color: Int) extends Constraint {
    override def isValid(domain: Domain): Boolean = {
      val colorA = domain.solution(a);
      if (colorA > 0) colorA != color else true 
    }
    
    override def prune(domain: Domain, constraints: Set[Constraint]): Set[Constraint] = {
      Set()
    }
  }
  
  class Domain(nodeCount: Int) {
    var solution = new Array[Int](nodeCount)
    var invalidNodeColors = new Array[List[Int]](nodeCount)
  }
  
  /**
   * Class for representing the graph edges
   */
  class Graph(val file: String) {
    private val lines = Source.fromFile(file).getLines().map(
        line => line.trim.split("\\s") 
        match {
          case Array(a, b) => List[Int](a.toInt, b.toInt)
        }).toList;
    val nodeCount = lines(0)(0)
    val edgeCount = lines(0)(1)
    val edges = lines.tail
  }
}