import scala.reflect.ClassTag

class SparseBipartite[T] (implicit m : ClassTag[T]) extends SparseAdjMat[T] {
  private var p_nRightVertices = 0
  private var conflictMaxDegree = -1

  def create(nLeftVertices: Int,
             nRightVertices: Int,
             nEdges: Int,
             rowOffsets: Array[Int],
             rowNSucc: Array[Int],
             edgeSucc: Array[Int],
             edgeVals: Array[T]) : SparseBipartite[T] = {
    create(nLeftVertices, nEdges, rowOffsets, rowNSucc, edgeSucc, edgeVals)
    p_nRightVertices = nRightVertices
    this
  }

  def numRightVertices() = {
    p_nRightVertices
  }

  def numLeftVertices() = {
    p_nVertices
  }

  override def numNodes() = {
    p_nVertices + p_nRightVertices
  }


}
