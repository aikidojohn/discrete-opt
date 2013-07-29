
import scala.io.Source
import scala.collection.mutable.HashSet
import scala.compat.Platform
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer

object Solver3 {

  def main(args: Array[String]) : Unit = {
    val graph = new Graph(args(0))
    //println("nodes: " + graph.nodeCount + " edges: " + graph.edgeCount)
    graph.nodes.foreach(n => println(n._1 + ": { " + (n._2.children.foldLeft("")((z, c) => z + c.index + " "))  + "}"))
    solve(graph)
  }
  
  def solve(graph: Graph) : Unit = {
    val isDebug = false
    //construct the domain. colors(i) represents all possible colors node(i) can take.
    var colors = new Array[HashSet[Int]](graph.nodeCount)
    for (i <- 0 to graph.nodeCount - 1) {
      colors(i) = new HashSet()
      for(j <- 0 to graph.nodeCount -1) {
        colors(i) += j
      }
    }
    
    var solution = List[Int](0)
    val initailChoice = new IsEqualConstraint(0, 0, graph)
    //Inital constraints first node is color 0 and all connected nodes are pairwise not equal.
    var constraints = HashSet[Constraint](initailChoice, new PairwiseNotEqualConstraint(graph.edges, graph))
    
    var previousColors = colors.clone
    var lastConstraint = initailChoice;
    while (solution.length < graph.nodeCount) {
	   if (propogate(constraints, colors)) {
	     //make a choice
	     val choice = colors(solution.length).reduceLeft((a, b) =>  if (a < b) a else b)
	     if (isDebug) {
	       println("choosing " + solution.length + " = " +  choice)
	     }
	     lastConstraint = new IsEqualConstraint(solution.length, choice, graph)
	     //find the minimum element to use as our next choice
	     solution = solution :+ choice
	     //Add the constraint, then repeat
	     constraints.add(lastConstraint)
	   } 
	   else {
	     if (isDebug) {
	       println("Backtracking...")
	     }
	     //last choice was bad. Backtrack and remove it as a possibility
	     colors = previousColors;
	     //remove the last choice from the solution
	     solution = solution.dropRight(1);
	     //and remove it from the domain
	     colors(solution.length) = colors(solution.length).drop(1)
	     //remove the constraint
	     constraints = constraints - lastConstraint
	   }
	   
	   previousColors = colors.clone;
    }
    
    val colorsUsed = 1 + solution.reduceLeft((a, b) => if (a > b) a else b)
    println(colorsUsed + " " + 0)
    solution.foreach(node => print(node + " "))
  }
  
  def propogate(constraints: HashSet[Constraint], colors: Array[HashSet[Int]]): Boolean = {
    var continue = true
    while(continue) {
        val t0 = Platform.currentTime
        continue = false
	    val it = constraints.iterator;
	    while (it.hasNext) {
	      val t2 = Platform.currentTime
	      val c = it.next
	      if (!c.isValid(colors)) {
	        return false;
	      }
	      
	      continue = continue || c.prune(colors);
	    }
    }
    return true
  }
  
  abstract class Constraint {
    def isValid(colors: Array[HashSet[Int]]): Boolean
    def prune(colors: Array[HashSet[Int]]): Boolean
    def getPrunedNodes(): List[Node]
    def setPrunedNodes(nodes: List[Node]): Unit
  }
   
   case class IsEqualConstraint(index: Int, value: Int, graph: Graph) extends Constraint {
     var nodesTouched = List[Node]()
     
     override def isValid(colors: Array[HashSet[Int]]): Boolean = {
       return colors(index).contains(value)
     }
     
     override def prune(colors: Array[HashSet[Int]]): Boolean = {
       //check if already pruned and return false if pruned
       if (colors(index).size == 1 && colors(index).contains(value)) {
         nodesTouched = List[Node]()
         return false
       }
       //otherwise prune and return true
       colors(index) = HashSet(value)
       nodesTouched = List(graph.nodes(index))
       return true
     }
     
     override def getPrunedNodes(): List[Node] = {
       nodesTouched
     }
     
     override def setPrunedNodes(nodes: List[Node]): Unit = {
       //Don't care about nodes touched by other constraints
     }
   }
   
   case class PairwiseNotEqualConstraint(edges: List[List[Int]], graph: Graph) extends Constraint {
     var nodesToCheck = Queue[Node]()
     var nodesTouched = ListBuffer[Node]()
     
     override def isValid(colors: Array[HashSet[Int]]): Boolean = {
       for (i <- 0 to edges.length - 1) {
         val edge = edges(i)
         if (isValid(colors(edge(0)), colors(edge(1)))) {
           return true
         }
       }
       return false
     }
     
     /**
      * Returns true if some a(i) != b(j). False otherwise.
      */
     private def isValid(a: HashSet[Int], b: HashSet[Int]): Boolean = {
       val itr = a.iterator
       while (itr.hasNext) {
         val itrb = b.iterator
         val nextA = itr.next;
         while(itrb.hasNext) {
           if (nextA != itrb.next) {
             return true;
           }
         }
       }
       return false
     }
     
     //Prune is slow. Need to do some selective pruning. Perhaps storing the graph as a linked structure
     // and keeping track of which nodes were touched, so that we only prune those nodes would be more efficient
     override def prune(colors: Array[HashSet[Int]]): Boolean = {
       nodesTouched.clear
       var pruned = false;
       //Prune looks at all edges and if the color domain for a node has only 1 color, 
       //ensure the color domain for connected nodes does not have that color.
       for (i <- 0 to edges.length - 1) {
         val edge = edges(i)
         
         val colorsA = colors(edge(0));
         val colorsB = colors(edge(1));
         if (colorsA.size == 1) {
           val color = colorsA.iterator.next
           //create a new list for B without the single color in A
           pruned = colorsB.remove(color)
         } 
         else if (colorsB.size == 1) {
           val color = colorsB.iterator.next
           //create a new list for A without the single color in A
           pruned = colorsA.remove(color)
         }
       }
       return pruned
     }
     
     override def getPrunedNodes(): List[Node] = {
       nodesTouched.toList
     }
     
     override def setPrunedNodes(nodes: List[Node]): Unit = {
       nodes.foreach(n => nodesToCheck.enqueue(n))
     }
     
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
    val nodes = edges.foldLeft(Map[Int, Node]())((nodes, edge) => {
      var nextNodes = nodes;
      var n0 = nodes.getOrElse(edge(0), null)
      if (n0 == null) {
        n0 = new Node(edge(0))
        nextNodes = nextNodes + (n0.index -> n0)
      }
      var n1 = nodes.getOrElse(edge(1), null)
      if (n1 == null) {
        n1 = new Node(edge(1))
        nextNodes = nextNodes + (n1.index -> n1)
      }
      n0 += n1
      n1 += n0
      nextNodes
    }) 
  }
  
  /**
   * A Node to represent a graph as a linked structure
   */
  class Node(val index: Int) {
    private val connectedNodes = new HashSet[Node]()
    private var immuntableNodes = Set[Node]();
    
    def +=(node: Node): Node = {
      connectedNodes.add(node)
      immuntableNodes = connectedNodes.toSet
      return this
    }
    
    def add(node: Node): Node = {
      return this += node
    }
    
    def children: Set[Node] = {
      immuntableNodes
    }
  }
}