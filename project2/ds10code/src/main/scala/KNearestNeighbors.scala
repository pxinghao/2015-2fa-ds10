import java.util.Random

class KNearestNeighbors (k: Int) extends Model {
  var trainWordCounts : SparseBipartite[Int] = null
  var trainTrackGenreFlags : Array[Array[Boolean]] = null
  var trainInclude: Array[Boolean] = null
  var nGenres = 0

  def train(wordCounts: SparseBipartite[Int], trackGenreFlags: Array[Array[Boolean]], include: Array[Boolean], params: Params) : Unit = {
    trainWordCounts = wordCounts
    trainTrackGenreFlags = trackGenreFlags
    trainInclude = include
    nGenres = trackGenreFlags(0).length
  }

  def test(testWordCounts: SparseBipartite[Int], idx_test: Int) : Int = {
    val nTrainData_all = trainWordCounts.numLeftVertices()
    val distances : Array[Double] = Array.fill[Double](nTrainData_all)(Double.PositiveInfinity)

    var idx_train = 0
    var d = 0.0
    var nSucc_i, nSucc_idx = 0
    var ii, jj = 0

    val wcArray_train : Array[Double] = new Array[Double](trainWordCounts.numRightVertices())

    idx_train = 0
    while (idx_train < nTrainData_all){
      if (trainInclude(idx_train)) {
        distances(idx_train) = WordVectorDistances.l1Dist_binarynorm(trainWordCounts, idx_train, testWordCounts, idx_test)
      }
      idx_train += 1
    }

    // Sort and pick top k
    val topkIndices = distances.zipWithIndex.sortWith((a,b) => (a._1 > b._1)).take(k).map(_._2)

    // Vote genres
    val genreVotes : Array[Int] = new Array(nGenres)
    var i, j = 0
    while (i < k){
      j = 0
      while (j < nGenres){
        if (trainTrackGenreFlags(topkIndices(i))(j)){
          genreVotes(j) += 1
        }
        j += 1
      }
      i += 1
    }

    // Find best class
    var maxVotes = 0
    i = 0
    while (i < nGenres){
      if (genreVotes(i) > maxVotes) maxVotes = genreVotes(i)
      i += 1
    }
    var nTopGenres = 0
    val topGenres = new Array[Int](nGenres)
    i = 0
    while (i < nGenres){
      if (genreVotes(i) == maxVotes){
        topGenres(nTopGenres) = i
        nTopGenres += 1
      }
      i += 1
    }
    val winnerGenreIdx = (new Random()).nextInt(nTopGenres)

    topGenres(winnerGenreIdx)
  }

}
