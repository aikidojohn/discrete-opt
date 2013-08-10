import scala.io.Source
import scala.annotation.tailrec

object Solver {

  def main(args: Array[String]): Unit = {
    val graph = new Graph(args(0))
    println("nodes: " + graph.nodeCount)
    print("points: ");
    graph.nodes.foreach(node => print("{" + node._1 + ", " + node._2 + "} "))
    println();
    solve(graph)
  }

  
  def solve(graph: Graph): Unit = {
    //use greedy algorithm to find a circuit
    val startTour = (0 until graph.nodeCount).toArray
    val tour = kopt(startTour, 0, graph)
    tour.foreach(x => print(x + " "))
  }
  
  def kopt(startTour: Array[Int], startIndex: Int, graph: Graph): Array[Int] = {
    val tourDist = tourDistance(graph, startTour)
    koptR(startTour, tourDist, startTour.clone, tourDist, startIndex, graph)
  }
  
  @tailrec
  def koptR(tour: Array[Int], tourDist: Double, bestTour: Array[Int], bestTourDist: Double, t0: Int, graph: Graph): Array[Int] = {
    //do kopt
    //choose a node t0 and its edge x0 (t0, t1)
    val t1 = (t0 + 1) % tour.length
    //compute distance between
    val dist = distance(graph.nodes(tour(t0))._1, graph.nodes(tour(t1))._1)
    
    //choose an edge x2, (t1, t2) with d(x1) < d(x0)
    val shortEdge = findShorterEdge(tour(t1), tour((t1 + 1) % tour.length), dist, graph)
    
    if (shortEdge < 0) {
      return bestTour
    }
    else {
      val t2 = tour.indexOf(shortEdge)
      //by taking (t2-1 + length) % length, we can avoid writing an if else here for the case that t2 is 0.
      val t3 = (t2 - 1 + tour.length) % tour.length
      
      //comput new tour distance = dist - (d(t0 -> t1) + d(t2 -> t3)) + (d(t0 -> t3) + d(t1 -> t2)
      val dist01 = distance(graph.nodes(tour(t0))._1, graph.nodes(tour(t1))._1)
      val dist23 = distance(graph.nodes(tour(t2))._1, graph.nodes(tour(t3))._1)
      val dist03 = distance(graph.nodes(tour(t0))._1, graph.nodes(tour(t3))._1)
      val dist12 = distance(graph.nodes(tour(t1))._1, graph.nodes(tour(t2))._1)
      val nextDistance = tourDist - dist01 - dist23 + dist03 + dist12
      
      //make the swaps
      swap(t1, t3, tour)
      
      //tour is better if nextDistance < tourDist
      if (nextDistance < bestTourDist) {
        koptR(tour, nextDistance, tour.clone, nextDistance, t0, graph)
      } else {
        koptR(tour, nextDistance, bestTour, bestTourDist, t0, graph)
      }
    }
  }
  
  def swap(x: Int, y: Int, a: Array[Int]): Array[Int] = {
    val tmp = a(x)
    a(x) = a(y)
    a(y) = tmp
    return a
  }
  
  /**
   * Finds an edge from start that is shorter than dist, if one exists.
   * Returns -1 if a shorter edge is not found.
   */
  def findShorterEdge(start: Int, existing: Int, dist: Double, graph: Graph): Int = {
    val startPoint = graph.nodes(start)._1
    
	val edge = graph.nodes.find(p => (p._2 != existing && p._2 != start && distance(p._1, startPoint) < dist)).getOrElse(null)
	if (edge == null) {
	  return -1;
	}
    return edge._2;
  }
  
  /**
   * Compute the euclidean distance between a and b
   */
  def distance(a: (Double, Double), b: (Double,Double)): Double = {
    Math.sqrt((Math.pow((a._1 - b._1), 2) + Math.pow((a._2 - b._2), 2)))
  }
  
  /**
   * Compute the tour distance
   */
  def tourDistance(graph: Graph, tour: Array[Int]): Double = {
    tour.sliding(2).foldRight(0.0)((p, d) => d + distance( graph.nodes(p(0))._1, graph.nodes(p(1))._1))
  }
  
  /**
   * Class for representing the graph edges
   */
  class Graph(val file: String) {
    private val lines = Source.fromFile(file).getLines().map(
        line => line.trim.split("\\s") 
        match {
          case Array(a, b) => (a.toDouble, b.toDouble)
          case Array(a) => (a.toDouble, 0.0)
        }).toList;
    val nodeCount = lines(0)._1.toInt
    val nodes = lines.tail.zipWithIndex
  }
}