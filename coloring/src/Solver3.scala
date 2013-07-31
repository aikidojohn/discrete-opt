
import scala.Array.canBuildFrom
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.io.Source

object Solver3 {
  val isDebug = true
  
  def main(args: Array[String]) : Unit = {
    val graph = new Graph(args(0))
    //println("nodes: " + graph.nodeCount + " edges: " + graph.edgeCount)
    //graph.nodes.foreach(n => println(n._1 + ": { " + (n._2.children.foldLeft("")((z, c) => z + c.index + " "))  + "}"))
    solve(graph)
  }
  
  def solve(graph: Graph) : Unit = {    
    //Inital constraints first node is color 0 and all connected nodes are pairwise not equal.
    val startingConstraints = Set[Constraint](new SymetryBreakingConstraint(graph), new PairwiseNotEqualConstraint(graph.edges, graph))
    
    var sol = findSolutionRecursive(graph, startingConstraints)
    var solution = sol
    while(!sol.isEmpty) {
      val colorsUsed = 1 + sol.reduceLeft((a, b) => if (a > b) a else b)
      val constraint = new MaxColorsConstraint(colorsUsed - 1, graph)
      solution = sol
      if (isDebug) println("found solution with " + colorsUsed + ". Looking for better solution")
      sol = findSolutionRecursive(graph, startingConstraints + constraint)
    }
    val colorsUsed = 1 + solution.reduceLeft((a, b) => if (a > b) a else b)
    println(colorsUsed + " " + 0)
    solution.foreach(node => print(node + " "))
  }
  
  def findSolutionRecursive(graph: Graph, initialConstraints: Set[Constraint]): List[Int] = {
    val colors = new Array[HashSet[Int]](graph.nodeCount)
    for (i <- 0 to graph.nodeCount - 1) {
      colors(i) = new HashSet()
      for(j <- 0 to graph.nodeCount -1) {
        colors(i) += j
      }
    }
    
    //Inital constraints first node is color 0 and all connected nodes are pairwise not equal.
    var constraints = HashSet[Constraint]()
    constraints ++= initialConstraints
    
    findSolutionR(graph, constraints, colors,  List[Int]());
  }
  
  def findSolutionR(graph: Graph, constraints: HashSet[Constraint], colors: Array[HashSet[Int]], solution: List[Int]): List[Int] = {
     //do we have a solution
     if (solution.length == graph.nodeCount) {
       solution
     } else {
	     //Make a choice
         var solutionFound = List[Int]()
         val itr = colors(solution.length).iterator
         while (itr.hasNext && solutionFound.isEmpty) {
        	 val choice = itr.next
		     //if (isDebug) println("choosing " + solution.length + " = " +  choice)
		     
		     val lastConstraint = new IsEqualConstraint(solution.length, choice, graph)
		     //find the minimum element to use as our next choice
		     val nextConstraints = constraints + lastConstraint
		     val nextColors = colors.map(set => set.clone())
		     //propogate
		     val nextSolution = 
			     if (propogate(nextConstraints, nextColors)) {
			       findSolutionR(graph, nextConstraints, nextColors, solution :+ choice)
			     } 
			     else {
			       List[Int]()
			     }
		     
		     if (!nextSolution.isEmpty) {
		       solutionFound = nextSolution
		     }
         }
         solutionFound
     }
  }
  
  def propogate(constraints: HashSet[Constraint], colors: Array[HashSet[Int]]): Boolean = {
    var continue = true
    var nodesTouched = ListBuffer[Node]()
    while(continue) {
        continue = false
	    val it = constraints.iterator;
	    while (it.hasNext) {
	      val c = it.next
	      if (!c.isValid(colors)) {
	        return false;
	      }
	      c.setPrunedNodes(nodesTouched.toList);
	      continue |= c.prune(colors);
	      nodesTouched.appendAll(c.getPrunedNodes)
	      /*if (isDebug) {
	          println("continue = " + continue)
		      print("touched nodes " + c.getClass().getSimpleName() + ": ")
		      nodesTouched.foreach(n => print(n.index + " "))
		      println()
	      }*/
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
       if (a.size == 0 || b.size == 0) {
         return false
       }
       
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
     
     override def prune(colors: Array[HashSet[Int]]): Boolean = {
       nodesTouched.clear
       var pruned = false;
       //Prune looks at all edges and if the color domain for a node has only 1 color, 
       //ensure the color domain for connected nodes does not have that color.
       while (!nodesToCheck.isEmpty) {
         //for each node in the nodesToCheckQueue, check if the domain for that node is 1
         val n = nodesToCheck.dequeue
         val colorsA = colors(n.index)
         if (colorsA.size == 1) {
           //if the domain for the node is 1, then remove the color from the domain of all connected nodes
           // and add each of those nodes to the list of nodes that we need to check with this constraint
           val color = colorsA.iterator.next
           n.children.foreach(c => {
             val didPrune = colors(c.index).remove(color)
             if (didPrune) {
               // if (isDebug) println("pruned " + color + " from domain of " + c.index)
               nodesTouched += c 
             }
             pruned |= didPrune
           })
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
   
   case class MaxColorsConstraint(max: Int, graph: Graph) extends Constraint {
     var nodesTouched = ListBuffer[Node]()
     
     override def isValid(colors: Array[HashSet[Int]]): Boolean = {
       //return true if all variable domains have an element that is < max
       return colors.forall((set) => set.exists(e => e < max))
     }
     
     override def prune(colors: Array[HashSet[Int]]): Boolean = {
       nodesTouched.clear
       //remove all values greater than or equal max from all domains
       var pruned = false;
       var index = 0;
       colors.foreach(set => {
         val initialSize = set.size;
         set.retain( (elem) => elem < max)
         if (initialSize > set.size) {
           nodesTouched += graph.nodes(index)
           pruned = true;
         }
         index += 1
       })
       return pruned
     }
     
     override def getPrunedNodes(): List[Node] = {
       nodesTouched.toList
     }
     
     override def setPrunedNodes(nodes: List[Node]): Unit = {
       //Don't care about nodes touched by other constraints
     }
   }
   
   case class SymetryBreakingConstraint(graph: Graph) extends Constraint {
     var nodesTouched = ListBuffer[Node]()
     
     override def isValid(colors: Array[HashSet[Int]]): Boolean = {
       //always valid. Constraint only prunes
       return true
     }
     
     override def prune(colors: Array[HashSet[Int]]): Boolean = {
       nodesTouched.clear
       //remove all values greater than or equal max from all domains
       var pruned = false;
       var index = 0;
       colors.foreach(set => {
         val initialSize = set.size;
         set.retain( (elem) => elem <= index)
         if (initialSize > set.size) {
           nodesTouched += graph.nodes(index)
           pruned = true;
         }
         index += 1
       })
       return pruned
     }
     
     override def getPrunedNodes(): List[Node] = {
       nodesTouched.toList
     }
     
     override def setPrunedNodes(nodes: List[Node]): Unit = {
       //Don't care about nodes touched by other constraints
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