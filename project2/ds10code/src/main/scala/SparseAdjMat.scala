import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class SparseAdjMat[T] (implicit m: ClassTag[T]) {
  protected var p_nVertices = 0
  protected var p_nEdges = 0

  protected var p_rowOffsets = new Array[Int](0)
  protected var p_rowNSucc   = new Array[Int](0)
  protected var p_edgeSucc   = new Array[Int](0)
  protected var p_edgeVals   = new Array[T](0)

  protected var p_maxDeg = -1

  // TODO(stephentu): these accessors are a hack to help us upgrade from
  // a SparseAdjMat to a SparseBipartite

  def getRowOffsets = p_rowOffsets
  def getRowNSucc = p_rowNSucc
  def getEdgeSucc = p_edgeSucc
  def getEdgeVals = p_edgeVals

  def create(nVertices: Int, nEdges: Int, rowOffsets: Array[Int], rowNSucc: Array[Int], edgeSucc: Array[Int], edgeVals: Array[T]) = {
    p_nVertices  = nVertices
    p_nEdges     = nEdges
    p_rowOffsets = rowOffsets
    p_rowNSucc   = rowNSucc
    p_edgeSucc   = edgeSucc
    p_edgeVals   = edgeVals
    p_maxDeg     = -1
  }

  def nSuccessor(i: Int) = {
    p_rowNSucc(i)
  }

  def succ(i: Int, j: Int) = {
    p_edgeSucc(p_rowOffsets(i) + j)
  }

  /**
   * useful for testing
   */
  def successors(i: Int) : Seq[Int] = {
    var nSucc = nSuccessor(i)
    var j = 0
    val buf = new ArrayBuffer[Int]()
    while (j < nSucc) {
      buf += succ(i, j)
      j += 1
    }
    buf.toSeq
  }

  def edge(i: Int, j: Int) = {
    (p_edgeSucc(p_rowOffsets(i) + j), p_edgeVals(p_rowOffsets(i) + j))
  }

  def edgeVal(i: Int, j: Int) = {
    p_edgeVals(p_rowOffsets(i) + j)
  }

  def numNodes() = {
    p_nVertices
  }

  def numArcs() = {
    p_nEdges
  }

  def maxDeg() = {
    if (p_maxDeg == -1){
      var i = 0
      while (i < p_nVertices){
        p_maxDeg = math.max(p_maxDeg, p_rowNSucc(i))
        i += 1
      }
    }
    p_maxDeg
  }
}

