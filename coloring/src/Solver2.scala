import scala.io.Source
import scala.annotation.tailrec

object Solver2 {

  def main(args: Array[String]) : Unit = {
    println("Hello Scala")
    val graph = new Graph(args(0))
    println("nodes: " + graph.nodeCount + " edges: " + graph.edgeCount)
    solve(graph)
  }
  
  def solve(graph: Graph) : Unit = {
    //Create a stream which will stream out all possible graph colorings.
    def domain: Stream[List[Int]] = {
      val max = graph.nodeCount;
      
      def makeList(numberColors: Int): List[Int] = {
        var colors = 0 until numberColors toList
        var padLen = max - numberColors
        while(padLen > 0) {
          colors = 0 :: colors
          padLen = padLen -1
        }
        colors
      } 
      
      var gen = new PermutationGenerator(makeList(3))
      gen.toStream
    }
    
    @tailrec
    def enumerate(c: Int, itr: Iterator[List[Int]]): Unit = {
      if (c > 0) {
        val nextPerm = itr.next
        if (!nextPerm.isEmpty) {
        	println(nextPerm + "\t" + c)
        	enumerate(c-1, itr)
        }
      }
    }
    
    val itr = domain.iterator
    enumerate(1000, itr)
  }
  
  class ColorGenerator(val size: Int) {
    private val array = new Array[Int](size)
    for (i <- 0 to size - 1) {
      array(i) = 0;
    }
    
    var numberColors = 2;
    private def next(): List[Int] = {
      for (color <- 1 to numberColors -1) {
        val maxDuplicate = (size - numberColors + 1)/2
        
      }
      List()
    }
  }

  /**
   * A class to list all permutations of a list as a stream
   */
  class PermutationGenerator(val list: List[Int]) {
    private val array = list.toArray

    private def nextPermutation(): List[Int] = {
      //Find the largest index k such that a[k] < a[k + 1].
      var k = array.length - 2
      while (k >= 0 && array(k) >= array(k + 1)) {
        k = k - 1
      }
      //If no such index exists, the permutation is the last permutation.
      if (k < 0) {
        return Nil
      }
      //Find the largest index l such that a[k] < a[l]. Since k + 1 is such an index, l is well defined and satisfies k < l.
      var l = array.length - 1
      while (array(k) >= array(l)) {
        l = l - 1
      }

      swap(k, l, array)
      reverse(k + 1, array);
      array.toList
    }
    
    private def swap(a: Int, b: Int, array: Array[Int]): Unit = {
      val s = array(a)
      array(a) = array(b)
      array(b) = s
    }

    @tailrec
    private def reverseRange(from: Int, to: Int, array: Array[Int]): Unit = {
      if (from < to) {
        swap(from, to, array)
        reverseRange(from + 1, to - 1, array)
      }
    }

    private def reverse(from: Int, array: Array[Int]): Unit = {
      reverseRange(from, array.length - 1, array);
    }

    private def stream(last: List[Int]): Stream[List[Int]] = last #:: stream(nextPermutation()) 
    
    /**
     * Lazily generate the permutations
     */
    def toStream(): Stream[List[Int]] = {
      stream(array.toList)
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
  }
}