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
//      if (tags(i)(0) == "country" || tags(i)(0) == "Alt-country" || tags(i)(0) == "classic country" || tags(i)(0) == "country rock") isCountry = true
      if (tags(i)(0) == "country") isCountry = true
//      if (tags(i)(0).toLowerCase contains "country") isCountry = true
      i += 1
    }

    isCountry
  }

  def isPopGenre(tags: Array[List[String]]) : Boolean = {
    var i = 0
    var isPop = false

    isPop = false
    i = 0
    while (i < tags.length){
      if (tags(i)(0) == "pop") isPop = true
      i += 1
    }

    isPop
  }

  def isHipHopGenre(tags: Array[List[String]]) : Boolean = {
    var i = 0
    var isHipHop = false

    isHipHop = false
    i = 0
    while (i < tags.length){
//      if (tags(i)(0) == "Hip-Hop" || tags(i)(0) == "hip hop") isHipHop = true
      if (tags(i)(0) == "Hip-Hop") isHipHop = true
//      if ((tags(i)(0).toLowerCase() contains "hip") && (tags(i)(0).toLowerCase() contains "hop")) isHipHop = true
      i += 1
    }

    isHipHop
  }

  def main(args: Array[String]) : Unit = {

    var i, j = 0


    // Parse musicXmatch data
    val mxmdata = FileParsers.parse_musicXmatch(new File("../data/mxm_dataset.txt"))
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
//    genreTests(0) = isPopGenre

    val trackGenreFlags: Array[Array[Boolean]] = Array.fill[Array[Boolean]](trackIDs.length)(Array.fill[Boolean](nGenres)(false))
    val trackGenre : Array[Int] = Array.fill[Int](trackIDs.length)(-1)
    val trackTitle : Array[String] = Array.fill[String](trackIDs.length)("")
    val trackArtist : Array[String] = Array.fill[String](trackIDs.length)("")

    val countCommon: AtomicInteger = new AtomicInteger(0)

//    val tagMap: ConcurrentHashMap[String, Map[String, Any]] = new ConcurrentHashMap[String, Map[String, Any]]()

    val lastfmFiles = FileParsers.recursiveListFiles(new File("../data/lastfm_test/"), new Regex(".json"))
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

//          tagMap(f.getName.dropRight(5)) = lastfmTrackTags

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
          trackTitle(i)  = lastfmData(lastfmFilenameMap(trackIDs(i))).getOrElse("title", "").toString
          trackArtist(i) = lastfmData(lastfmFilenameMap(trackIDs(i))).getOrElse("artist", "").toString
          println(s"Track ${trackIDs(i)} [${lastfmData(lastfmFilenameMap(trackIDs(i))).getOrElse("title", "")}] is of genre $j")
          trackGenre(i) = j
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
        trackGenre(i) = -1
      }
      i += 1
    }

    workerThreads.shutdown()

    // Do LOOCV k-NN
    /*
    var countCorrect = 0
    var countWrong = 0
    val include: Array[Boolean] = Array.fill[Boolean](trackIDs.length)(false)
    i = 0
    while (i < trackIDs.length) {
      if (trackGenre(i) != -1) {
        println(s"trackGenre($i) = ${trackGenre(i)}")
        include(i) = true
      }
      i += 1
    }

    i = 0
    while (i < trackIDs.length) {
      val trueClass = trackGenre(i)

      if (trueClass != -1) {
        include(i) = false

        val knn = new KNearestNeighbors(5)
        knn.train(wordCounts, trackGenre, include, null)
        val estimatedClass = knn.test(wordCounts, i)

//        val nb = new NaiveBayes
//        nb.train(wordCounts, trackGenre, include, new NaiveBayesParams(1.0))
//        val estimatedClass = nb.test(wordCounts, i)

        if (trueClass == estimatedClass) {
          countCorrect += 1
        } else {
          countWrong += 1
        }
        println(s"${trueClass}\t$estimatedClass\t$countCorrect\t$countWrong\t${countCorrect.toDouble / (countCorrect + countWrong).toDouble * 100.0}%")
        include(i) = true
      }
      i += 1
    }
    */

    val genreOutput = new PrintWriter(new File("../data/genredata.dat"))
    i = 0
    while (i < trackIDs.length) {
      if (trackGenre(i) != -1) {
        genreOutput.print(s"${trackGenre(i) + 1}\t")
      }
      i += 1
    }
    genreOutput.println()
    genreOutput.close()
    println("Done with genre output")

    val titleOutput = new PrintWriter(new File("../data/titledata.dat"))
    i = 0
    while (i < trackIDs.length) {
      if (trackGenre(i) != -1) {
        titleOutput.print(s"${trackTitle(i)}\n")
      }
      i += 1
    }
    titleOutput.println()
    titleOutput.close()
    println("Done with title output")

    val artistOutput = new PrintWriter(new File("../data/artistdata.dat"))
    i = 0
    while (i < trackIDs.length) {
      if (trackGenre(i) != -1) {
        artistOutput.print(s"${trackArtist(i)}\n")
      }
      i += 1
    }
    artistOutput.println()
    artistOutput.close()
    println("Done with artist output")

    val wcOutput = new PrintWriter(new File("../data/wordcounts.dat"))
    i = 0
    var countData = 0
    while (i < trackIDs.length) {
      if (trackGenre(i) != -1) {
        countData += 1
        var ii = 0
        while (ii < wordCounts.nSuccessor(i)) {
          wcOutput.println(s"$countData\t${wordCounts.succ(i, ii) + 1}\t${wordCounts.edgeVal(i, ii)}")
          ii += 1
        }
      }
      i += 1
    }
    wcOutput.println(s"$countData\t${wordCounts.numRightVertices()}\t0")
    wcOutput.close()
    println("Done with word-count output")

  }

}
