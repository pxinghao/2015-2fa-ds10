abstract class Params

abstract class Model {
  def train(wordCounts: SparseBipartite[Int], trackGenreFlags: Array[Array[Boolean]], include: Array[Boolean], params: Params) : Unit
  
  def test(wordCounts: SparseBipartite[Int], idx: Int) : Int
}
