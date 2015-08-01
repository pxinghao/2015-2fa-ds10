class NaiveBayesParams(priorCount: Double) extends Params {
  def getPriorCount : Double = priorCount
}

class NaiveBayes extends Model {
  var nGenres = 0
  var nWords = 0

  var prior : Array[Double] = null
  var likelihoods: Array[Array[Double]] = null

  def train(wordCounts: SparseBipartite[Int], classLabels: Array[Int], include: Array[Boolean], params: Params) : Unit = {
    nGenres = classLabels.max + 1
    nWords = wordCounts.numRightVertices()

    val priorCount = params.asInstanceOf[NaiveBayesParams].getPriorCount

    var i, j = 0
    var nSucc = 0

    var sumCount = 0.0


    // Compute priors
    var sumPriors = 0
    prior = Array.fill[Double](nGenres)(0.0)
    i = 0
    while (i < classLabels.length){
      if (include(i)) {
        sumPriors += 1
        prior(classLabels(i)) += 1
      }
      i += 1
    }
    i = 0
    while (i < nGenres){
      prior(i) /= sumPriors.toDouble
      i += 1
    }

    // Count number of words
    likelihoods = Array.fill[Array[Double]](nGenres)(Array.fill[Double](nWords)(priorCount))
    i = 0
    while (i < wordCounts.numLeftVertices()){
      if (include(i)) {
        nSucc = wordCounts.nSuccessor(i)
        j = 0
        while (j < nSucc) {
          likelihoods(classLabels(i))(wordCounts.succ(i, j)) += wordCounts.edgeVal(i, j)
          j += 1
        }
      }
      i += 1
    }

    // Normalize likelihoods
    i = 0
    while (i < nGenres){
      sumCount = 0.0
      j = 0
      while (j < nWords){
        sumCount += likelihoods(i)(j)
        j += 1
      }
      j = 0
      while (j < nWords){
        likelihoods(i)(j) /= sumCount
//        println(s"likelhoods($i)($j) = ${likelihoods(i)(j)}")
        j += 1
      }
      i += 1
    }

  }

  def test(wordCounts: SparseBipartite[Int], idx: Int) : Int = {
    var i, j = 0

    val nSucc = wordCounts.nSuccessor(idx)

    val logProbs : Array[Double] = new Array[Double](nGenres)
    (0 until nGenres).foreach(gi => {
      logProbs(gi) = prior(gi)
    })

    i = 0
    while (i < nSucc){
      j = 0
      while (j < nGenres) {
        logProbs(j) += wordCounts.edgeVal(idx, i) * math.log(likelihoods(j)(wordCounts.succ(idx, i)))
        j += 1
      }
      i += 1
    }

    var maxLogProbGenre = 0
    i = 0
    while (i < nGenres){
      if (logProbs(maxLogProbGenre) < logProbs(i)){
        maxLogProbGenre = i
      }
      i += 1
    }

    maxLogProbGenre
  }

}
