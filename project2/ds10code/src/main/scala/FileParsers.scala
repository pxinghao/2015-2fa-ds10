import java.io.File

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.json._

object FileParsers {

  def recursiveListFiles(f: File, r: Regex): Array[File] = {
    val these = f.listFiles
    val good = these.filter(f => r.findFirstIn(f.getName).isDefined)
    good ++ these.filter(_.isDirectory).flatMap(recursiveListFiles(_,r))
  }

  def parse_lastfm(f: File) : Map[String,Any] = {
    val lines : Array[String] = Source.fromFile(f).getLines().toArray
    val result = JSON.parseFull(lines(0))
    val r : Map[String,Any] = result match {
      case Some(e) => e.asInstanceOf[Map[String,Any]]
      case None => null
    }

    r
  }

  def parse_musicXmatch(f: File, doBinary: Boolean = false) = {
    val lines : Array[String] = Source.fromFile(f).getLines().toArray

    var wordsArray : Array[String] = null
    val trackIDs : Array[String] = new Array[String](lines.length)
    val trackIDMap : HashMap[String, Int] = new HashMap[String, Int]()

    val wordCounts : SparseBipartite[Int] = new SparseBipartite[Int]()

    var nTracks = 0
    var nWordCounts = 0
    var maxWordID = 0
    val wcOffset : ArrayBuffer[Int] = new ArrayBuffer[Int]()
    val wcTrackNWordCount : ArrayBuffer[Int] = new ArrayBuffer[Int]()
    val wcWord : ArrayBuffer[Int] = new ArrayBuffer[Int]()
    val wcCount : ArrayBuffer[Int] = new ArrayBuffer[Int]()

    var i = 0
    var j = 0

    i = 0
    while (i < lines.length){
      if (lines(i)(0) == '#'){
        // ignore comment
      }else if (lines(i)(0) == '%'){
        // Dictionary
        wordsArray = lines(i).drop(1).split(',')
      }else{
        val terms = lines(i).split(',')

        trackIDs(nTracks) = terms(0)
        trackIDMap(terms(0)) = nTracks

        val thisTrackNWordCount = terms.length - 2

        nTracks += 1
        wcOffset += nWordCounts
        nWordCounts += thisTrackNWordCount
        wcTrackNWordCount += thisTrackNWordCount

        j = 2
        while (j < terms.length){
          val wc = terms(j).split(':')
          val w = wc(0).toInt - 1       // 0-indexing
          val c = wc(1).toInt
          maxWordID = math.max(maxWordID, w)
          wcWord += w
          wcCount += c
          j += 1
        }

      }
      i += 1
    }

    wordCounts.create(nTracks, maxWordID, nWordCounts, wcOffset.toArray, wcTrackNWordCount.toArray, wcWord.toArray, wcCount.toArray)

    (wordsArray, trackIDs, trackIDMap, wordCounts)
  }

}
