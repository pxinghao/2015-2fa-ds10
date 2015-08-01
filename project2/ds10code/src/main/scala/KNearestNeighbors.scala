import java.util.Random

class KNearestNeighbors (k: Int) extends Model {
  var trainWordCounts : SparseBipartite[Int] = null
  var trainClassLabels : Array[Int] = null
  var trainInclude: Array[Boolean] = null
  var nGenres = 0

  def train(wordCounts: SparseBipartite[Int], classLabels: Array[Int], include: Array[Boolean], params: Params) : Unit = {
    trainWordCounts = wordCounts
    trainClassLabels = classLabels
    trainInclude = include
    nGenres = classLabels.max + 1
  }

  def test(testWordCounts: SparseBipartite[Int], idx_test: Int) : Int = {
    val nTrainData_all = trainWordCounts.numLeftVertices()
    val distances : Array[Double] = Array.fill[Double](nTrainData_all)(Double.PositiveInfinity)

    var idx_train = 0

    idx_train = 0
    while (idx_train < nTrainData_all){
      if (trainInclude(idx_train)) {
        distances(idx_train) = WordVectorDistances.l1Dist(trainWordCounts, idx_train, testWordCounts, idx_test)
      }
      idx_train += 1
    }

    // Sort and pick top k
    val distanceZipIndex = (0 until nTrainData_all).map(ti => (distances(ti), ti))
    val sortedDistancesZipIndx = distanceZipIndex.sortWith((a,b) => (a._1 < b._1))
    val topkIndices = sortedDistancesZipIndx.take(k).map(_._2)

    // Vote genres
    val genreVotes : Array[Int] = new Array(nGenres)
    var i, j = 0
    while (i < k){
      genreVotes(trainClassLabels(topkIndices(i))) += 1
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
