object WordVectorDistances {
  def l1Dist(trainWordCounts: SparseBipartite[Int], idx_train: Int, testWordCounts: SparseBipartite[Int], idx_test: Int) : Double = {
    var d = 0.0

    val nSucc_train = trainWordCounts.nSuccessor(idx_train)
    val nSucc_test  = testWordCounts.nSuccessor(idx_test)

    var i_train, i_test = 0

    i_train = 0
    i_test  = 0
    while (i_train < nSucc_train && i_test < nSucc_test){
      val word_train : Int = trainWordCounts.succ(idx_train, i_train)
      val word_test  : Int = testWordCounts.succ(idx_test, i_test)
      if (word_train == word_test){
        d += math.abs(trainWordCounts.edgeVal(idx_train, i_train) - testWordCounts.edgeVal(idx_test, i_test)).toDouble
        i_train += 1
        i_test += 1
      }else if (word_train < word_test){
        d += math.abs(trainWordCounts.edgeVal(idx_train, i_train)).toDouble
        i_train += 1
      }else if (word_train > word_test){
        d += math.abs(testWordCounts.edgeVal(idx_test, i_test)).toDouble
        i_test += 1
      }
    }

    d
  }

  def l1Dist_norm(trainWordCounts: SparseBipartite[Int], idx_train: Int, testWordCounts: SparseBipartite[Int], idx_test: Int) : Double = {
    var d = 0.0

    val nSucc_train = trainWordCounts.nSuccessor(idx_train)
    val nSucc_test  = testWordCounts.nSuccessor(idx_test)

    var i_train, i_test = 0

    var sum_train = 0
    var sum_test = 0
    i_train = 0
    while (i_train < nSucc_train){
      sum_train += trainWordCounts.edgeVal(idx_train, i_train)
      i_train += 1
    }
    i_test = 0
    while (i_test < nSucc_test){
      sum_test += testWordCounts.edgeVal(idx_test, i_test)
      i_test += 1
    }

    i_train = 0
    i_test  = 0
    while (i_train < nSucc_train && i_test < nSucc_test){
      val word_train : Int = trainWordCounts.succ(idx_train, i_train)
      val word_test  : Int = testWordCounts.succ(idx_test, i_test)
      if (word_train == word_test){
        d += math.abs(trainWordCounts.edgeVal(idx_train, i_train).toDouble / sum_train.toDouble - testWordCounts.edgeVal(idx_test, i_test).toDouble / sum_test.toDouble)
        i_train += 1
        i_test += 1
      }else if (word_train < word_test){
        d += math.abs(trainWordCounts.edgeVal(idx_train, i_train).toDouble / sum_train.toDouble)
        i_train += 1
      }else if (word_train > word_test){
        d += math.abs(testWordCounts.edgeVal(idx_test, i_test).toDouble / sum_test.toDouble)
        i_test += 1
      }
    }

    d
  }

  def l1Dist_binarynorm(trainWordCounts: SparseBipartite[Int], idx_train: Int, testWordCounts: SparseBipartite[Int], idx_test: Int) : Double = {
    var d = 0.0

    val nSucc_train = trainWordCounts.nSuccessor(idx_train)
    val nSucc_test  = testWordCounts.nSuccessor(idx_test)

    var i_train, i_test = 0

    var sum_train = 0
    var sum_test = 0
    i_train = 0
    while (i_train < nSucc_train){
      sum_train += 1 //trainWordCounts.edgeVal(idx_train, i_train)
      i_train += 1
    }
    i_test = 0
    while (i_test < nSucc_test){
      sum_test += 1 //testWordCounts.edgeVal(idx_test, i_test)
      i_test += 1
    }

    i_train = 0
    i_test  = 0
    while (i_train < nSucc_train && i_test < nSucc_test){
      val word_train : Int = trainWordCounts.succ(idx_train, i_train)
      val word_test  : Int = testWordCounts.succ(idx_test, i_test)
      if (word_train == word_test){
        d += math.abs(1.0 / sum_train.toDouble - 1.0 / sum_test.toDouble)
        i_train += 1
        i_test += 1
      }else if (word_train < word_test){
        d += math.abs(1.0 / sum_train.toDouble)
        i_train += 1
      }else if (word_train > word_test){
        d += math.abs(1.0 / sum_test.toDouble)
        i_test += 1
      }
    }

    d
  }

}
