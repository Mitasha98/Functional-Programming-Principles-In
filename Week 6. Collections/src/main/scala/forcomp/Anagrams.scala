package forcomp

import java.io.Serializable

import scala.collection.immutable


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = loadDictionary

  def occurrencesWord(occurrences: Occurrences): Word = {
    occurrences.foldLeft("")(_ + _._1.toString)
  }
  def wordOccurrences(w: Word): Occurrences = {
    w.groupBy(c => c.toLower).map { case (x, y) => (x, y.length) }.toList.sorted
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    if (s.isEmpty) Nil
    else s.map(wordOccurrences).reduce(mergedOccurrences)
  }

  def mergedOccurrences(lhs: Occurrences, rhs: Occurrences): Occurrences = {
    def merge(lhs: Occurrences, rhs: Occurrences, accl: Occurrences): Occurrences = {
      (lhs, rhs) match {
        case (Nil, Nil) => accl.reverse
        case (_, Nil) => accl.reverse ::: lhs
        case (Nil, _) => accl.reverse ::: rhs
        case (x :: xs, y :: ys) =>
          if (x._1 < y._1) merge(lhs.tail, rhs, x :: accl)
          else if (x._1 > y._1) merge(lhs, rhs.tail, y :: accl)
          else merge(lhs.tail, rhs.tail, (x._1, x._2 + y._2) :: accl)
      }
    }
    merge(lhs, rhs, List())
  }

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(wordOccurrences)

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    val occurrences = wordOccurrences(word)
    dictionaryByOccurrences.getOrElse(occurrences, Nil)
  }

  def combinations(occurrences: Occurrences): List[Occurrences] = {
    val s = occurrencesToString(occurrences)
    val subseqs = subSequences(s).toSet
    subseqs.map(wordOccurrences).toList
  }

  def subSequences(s: String): List[String] = {
    def combinations(s: String, accl: List[String]): List[String] = {
      if (s.isEmpty) accl
      else {
        val lists = accl.map(_ + s.head)
        combinations(s.tail, lists ::: accl)
      }
    }

    val empty = List("")
    combinations(s, empty)
  }

  def occurrencesToString(occurrences: Occurrences): String = {
    occurrences.foldLeft("")((s, e) => s + e._1.toString * e._2)
  }

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xmap = x.toMap
    y.foldLeft(xmap) {
      case (map, (char, frequency)) =>
        val remainder = map(char) - frequency
        if (remainder == 0) map - char
        else map + (char -> remainder)
    }.toList.sorted
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val senOcc = sentenceOccurrences(sentence)
    val occurrences = occurrencesCombinations(senOcc)
    val sentences = occurrences.flatMap { occurrence =>
      val sentences = occurrence.map(dictionaryByOccurrences)
      elementCombinations(sentences)
    }
    sentences
  }

  def occurrencesCombinations(occurrences: Occurrences): List[List[Occurrences]] = {
    if (occurrences.isEmpty) {
      List(List())
    }
    else
      for {
        left <- combinations(occurrences).filter(dictionaryByOccurrences.contains)
        remainder = subtract(occurrences, left)
        right <- occurrencesCombinations(remainder)
        if sumsEqual(remainder, right)
      } yield left :: right
  }

  def sumsEqual(occurrences: Occurrences, ol: List[Occurrences]): Boolean = {
    if (ol.isEmpty) occurrences.isEmpty
    else ol.reduce(mergedOccurrences) == occurrences
  }

  def elementCombinations(lists: List[List[String]]): List[List[String]] = {
    if (lists.isEmpty) List(List())
    else elementCombinations(lists.tail).flatMap(xs => lists.head.map(_ :: xs))
  }
}

