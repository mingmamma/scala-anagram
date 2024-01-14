val occur = List(('a', 2), ('b', 2))

val occur1 = occur
.flatMap((char, count) => (0 to count)
.map(num => (char, num))
.filter((char, num) => num != 0))

val occur2 = occur
.flatMap((char, count) => (0 to count)
.map(num => (char, num))
.filter((char, num) => num != 0))

occur1.flatMap((char1, num1) =>
    occur2.map((char2, num2) =>
        List((char1, num1), (char2, num2))))

import scala.io.{ Codec, Source }

type Word = String
type Occurrences = List[(Char, Int)]
type Sentence = List[Word]


// following the approach from the nQueen example in the lecture
def combinations(occurrences: Occurrences): List[Occurrences] =
    if occurrences.isEmpty then List(List())
    else
        combinations(occurrences.tail).flatMap(previousComb =>
            (0 to occurrences.head._2).map(headItemNum =>
                if headItemNum == 0 then previousComb
                else (occurrences.head._1, headItemNum) :: previousComb
            )
        )

combinations(List(('a', 2), ('b', 2)))

val dictionary: List[Word] = Dictionary.loadDictionary

object Dictionary:
  def loadDictionary: List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    catch
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    finally
      wordstream.close()

lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(word => wordOccurrences(word))

def wordOccurrences(w: Word): Occurrences =
    w.groupBy(char => char.toLower)
     .toList
     .sortBy((char, chars) => char)(Ordering[Char])
     .map((char, chars) => (char, chars.length()))    

def sentenceOccurrences(s: Sentence): Occurrences =
    val sentenceStr = s.mkString("") 
    wordOccurrences(sentenceStr)    

def subtract(x: Occurrences, y: Occurrences): Occurrences =
    val yMap = y.toMap
    x.toMap
     .foldLeft(Map.empty[Char, Int])((acc, kv) => 
      yMap.get(kv._1) match
        case None => acc.updated(kv._1, kv._2)
        case Some(yNum) =>
          if yNum == kv._2 then acc
          else acc.updated(kv._1, kv._2 - yNum))
     .toList
     .sortBy((char, num)=> char)    

def sentenceAnagrams(sentence: Sentence): List[Sentence] =
    def sentenceAnagramFromOccurance(sentenceOccurance: Occurrences): List[Sentence] =
    if sentenceOccurance.isEmpty then List(List.empty)
    else
        for 
        subSetOccur <- combinations(sentenceOccurance)
        anagramOfSubSetOccur <- dictionaryByOccurrences.getOrElse(subSetOccur, Nil)
        anagramsFromRemaining <- sentenceAnagramFromOccurance(subtract(sentenceOccurance, subSetOccur))
        yield anagramOfSubSetOccur :: anagramsFromRemaining
        

    sentenceAnagramFromOccurance(sentenceOccurrences(sentence))


//   def sentenceAnagramsMemo(sentence: Sentence): List[Sentence] =
//       def sentenceAnagramFromOccurance(sentenceOccurance: Occurrences, memo: Map[Occurrences, List[Word]]): List[Sentence] =
//         if sentenceOccurance.isEmpty then List(List.empty)
//         else
//             combinations(sentenceOccurance).flatMap(subSetOccur =>
//                 // if (memo.contains(subSetOccur)) {
//                 //   val anagramsOfSubSetOccur = memo(subSetOccur)
//                 // }
//                 // else {
//                 //   val anagramsOfSubSetOccur = dictionaryByOccurrences.getOrElse(subSetOccur, Nil)
//                 //   // memo.updated(subSetOccur, dictionaryByOccurrences.getOrElse(subSetOccur, Nil))
//                 // }
              
//               val anagramsOfSubSetOccur = 
//                 if (memo.contains(subSetOccur)) then memo(subSetOccur)
//                 else dictionaryByOccurrences.getOrElse(subSetOccur, Nil)
//               anagramsOfSubSetOccur.flatMap(anagramOfSubSetOccur =>
//                 sentenceAnagramFromOccurance(subtract(sentenceOccurance, subSetOccur), memo).map(anagramsFromRemaining =>
//                   anagramOfSubSetOccur :: anagramsFromRemaining
//                 )
//               )
//             )
//       sentenceAnagramFromOccurance(sentenceOccurrences(sentence), Map())

sentenceAnagrams(List("tea"))

sentenceAnagrams(List("is", "Lev"))

sentenceAnagrams(List("yes", "man"))

sentenceAnagrams(List("iloveyou"))

sentenceAnagrams(List("Linux", "rulez"))

sentenceAnagrams(List("Linux", "rulez"))




        