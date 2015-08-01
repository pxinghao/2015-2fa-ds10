import java.util.concurrent.atomic.{AtomicInteger, AtomicIntegerArray}

import scala.collection.JavaConversions._

import java.io.{PrintWriter, File}
import java.util.concurrent.{ConcurrentHashMap, Callable, Executors, ExecutorService}

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.util.matching.Regex

object Driver {

  def isCountryGenre(tags: Array[List[String]]) : Boolean = {
    var i = 0
    var isCountry = false

    isCountry = false
    i = 0
    while (i < tags.length){
      if (tags(i)(0) == "country") isCountry = true
      i += 1
    }

    isCountry
  }

  def isHipHopGenre(tags: Array[List[String]]) : Boolean = {
    var i = 0
    var isCountry = false

    isCountry = false
    i = 0
    while (i < tags.length){
      if (tags(i)(0) == "Hip-Hop") isCountry = true
      i += 1
    }

    isCountry
  }

  def main(args: Array[String]) : Unit = {

    var i, j = 0


    // Parse musicXmatch data
    val mxmdata = FileParsers.parse_musicXmatch(new File("../data/mxm_dataset_train.txt"))
    val wordsArray: Array[String] = mxmdata._1
    val trackIDs: Array[String] = mxmdata._2
    val trackIDMap: mutable.HashMap[String, Int] = mxmdata._3
    val wordCounts: SparseBipartite[Int] = mxmdata._4

    val wordsOutput = new PrintWriter(new File("../data/words.dat"), "UTF-8")
    wordsArray.foreach(w => wordsOutput.println(s"$w"))
    wordsOutput.close()
    println("Done with words output")

    // Parse lastfm data to extract songs for which we have lyrics (i.e. in musicXmatch data),
    // and belongs to either country or Hip-Hop
    val nGenres = 2
    val genreCounts: AtomicIntegerArray = new AtomicIntegerArray(nGenres)
    val genreTests: Array[Array[List[String]] => Boolean] = new Array[Array[List[String]] => Boolean](nGenres)
    genreTests(0) = isCountryGenre
    genreTests(1) = isHipHopGenre

    val trackGenreFlags: Array[Array[Boolean]] = Array.fill[Array[Boolean]](trackIDs.length)(Array.fill[Boolean](nGenres)(false))

    val countCommon: AtomicInteger = new AtomicInteger(0)

    val tagMap: HashMap[String, Map[String, Any]] = new HashMap[String, Map[String, Any]]()

    val lastfmFiles = FileParsers.recursiveListFiles(new File("../data/lastfm_subset/"), new Regex(".json"))
    val lastfmData: Array[Map[String, Any]] = new Array[Map[String, Any]](lastfmFiles.length)
    val lastfmFilenameMap = new mutable.HashMap[String, Int]()
    (0 until lastfmFiles.length).foreach(fi => lastfmFilenameMap(lastfmFiles(fi).getName.dropRight(5)) = fi)

    val nThreads = Runtime.getRuntime.availableProcessors
    val workerThreads: ExecutorService = Executors.newFixedThreadPool(nThreads)

    val tasks_processLastfmData = (0 until nThreads).map(threadID => new Callable[Unit] {
      override def call() = {
        var fi = threadID

        var i = 0

        fi = threadID
        while (fi < lastfmFiles.length) {
          val f = lastfmFiles(fi)
          val lastfmTrackTags = FileParsers.parse_lastfm(f)
          lastfmData(fi) = lastfmTrackTags

          tagMap(f.getName.dropRight(5)) = lastfmTrackTags

          if (trackIDMap.contains(f.getName.dropRight(5))) {
            val trackID = trackIDMap.get(f.getName.dropRight(5)).get
            countCommon.incrementAndGet()
            val tags = lastfmTrackTags.get("tags").get.asInstanceOf[List[List[String]]].toArray
            //println(s"$countCommon)\t${f.getName.dropRight(5)}:\t${lastfmTrackTags.getOrElse("title","")}")
            var hasValidGenre = false
            i = 0
            while (i < nGenres) {
              if (genreTests(i)(tags)) {
                trackGenreFlags(trackID)(i) = true
                genreCounts.incrementAndGet(i)
                hasValidGenre = true
              }
              i += 1
            }
            /*
            if (hasValidGenre) {
              print(s"$countCommon [")
              i = 0
              while (i < nGenres) {
                print(s"${genreCounts.get(i)}")
                if (i < nGenres - 1) print(", ")
                i += 1
              }
              println(s"]\t${f.getName.dropRight(5)}:\t${lastfmTrackTags.getOrElse("title", "")}")
            }
            */
          }
          fi += nThreads
        }
      }
    })
    workerThreads.invokeAll(tasks_processLastfmData)

    i = 0
    while (i < trackIDs.length) {
      j = 0
      var count = 0
      while (j < nGenres) {
        if (trackGenreFlags(i)(j)) {
          //println(s"Track ${trackIDs(i)} [${lastfmData(lastfmFilenameMap(trackIDs(i))).getOrElse("title", "")}] is of genre $j")
          count += 1
        }
        j += 1
      }
      if (count > 1) {
        // Remove this point
        j = 0
        while (j < nGenres) {
          trackGenreFlags(i)(j) = false
          j += 1
        }
      }
      i += 1
    }

    workerThreads.shutdown()

    // Do LOOCV 1-NN
    /*
    var countCorrect = 0
    var countWrong = 0
    val include: Array[Boolean] = Array.fill[Boolean](trackIDs.length)(false)
    i = 0
    while (i < trackIDs.length) {
      j = 0
      while (j < nGenres) {
        if (trackGenreFlags(i)(j)) {
          include(i) = true
        }
        j += 1
      }
      i += 1
    }

    i = 0
    while (i < trackIDs.length) {
      var validData = false
      j = 0
      while (j < nGenres && !validData) {
        if (trackGenreFlags(i)(j)) {
          validData = true
        }
        j += 1
      }

      if (validData) {
        var trueClass = -1
        j = 0
        while (j < nGenres) {
          if (trackGenreFlags(i)(j)) trueClass = j
          j += 1
        }
        include(i) = false

        //        val knn = new KNearestNeighbors(5)
        //        knn.train(wordCounts, trackGenreFlags, include, null)
        //        val estimatedClass = knn.test(wordCounts, i)

        val nb = new NaiveBayes
        nb.train(wordCounts, trackGenreFlags, include, new NaiveBayesParams(1.0))
        val estimatedClass = nb.test(wordCounts, i)

        if (trueClass == estimatedClass) {
          countCorrect += 1
        } else {
          countWrong += 1
        }
//        println(s"${trueClass}\t$estimatedClass\t$countCorrect\t$countWrong\t${countCorrect.toDouble / (countCorrect + countWrong).toDouble * 100.0}%")
        include(i) = true
      }
      i += 1
    }
    */


    val genreOutput = new PrintWriter(new File("../data/genredata.dat"))
    i = 0
    while (i < trackIDs.length) {
      var validData = false
      j = 0
      while (j < nGenres && !validData) {
        if (trackGenreFlags(i)(j)) {
          genreOutput.print(s"$j\t")
          validData = true
        }
        j += 1
      }
      i += 1
    }
    genreOutput.println()
    genreOutput.close()
    println("Done with genre output")

    val wcOutput = new PrintWriter(new File("../data/wordcounts.dat"))
    i = 0
    var countData = 0
    while (i < trackIDs.length) {
      var validData = false
      j = 0
      while (j < nGenres && !validData) {
        if (trackGenreFlags(i)(j)) {
          countData += 1
          var ii = 0
          while (ii < wordCounts.nSuccessor(i)) {
            wcOutput.println(s"$countData\t${wordCounts.succ(i, ii) + 1}\t${wordCounts.edgeVal(i, ii)}")
            ii += 1
          }
          validData = true
        }
        j += 1
      }
      i += 1
    }
    wcOutput.println(s"$countData\t${wordCounts.numRightVertices()}\t0")
    wcOutput.close()
    println("Done with word-count output")

  }

}
