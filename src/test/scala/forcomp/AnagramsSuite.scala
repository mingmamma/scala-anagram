package forcomp

class AnagramsSuite extends munit.FunSuite:
  import Anagrams.*

  test("wordOccurrences: abcd (3pts)") {
    assertEquals(wordOccurrences("abcd"), List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert (3pts)") {
    assertEquals(wordOccurrences("Robert"), List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e (5pts)") {
    assertEquals(sentenceOccurrences(List("abcd", "e")), List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: roBoT RobeRt") {
    assertEquals(sentenceOccurrences(List("rOBoT", "RobeRt")), List(('b', 2), ('e', 1), ('o', 3), ('r', 3), ('t', 2)))
  }  


  test("dictionaryByOccurrences.get: eat (10pts)") {
    assertEquals(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet), Some(Set("ate", "eat", "tea")))
  }


  test("wordAnagrams married (2pts)") {
    assertEquals(wordAnagrams("married").toSet, Set("married", "admirer"))
  }

  test("wordAnagrams player (2pts)") {
    assertEquals(wordAnagrams("player").toSet, Set("parley", "pearly", "player", "replay"))
  }



  test("subtract: lard - r (10pts)") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assertEquals(subtract(lard, r), lad)
  }


  test("combinations: [] (8pts)") {
    assertEquals(combinations(Nil), List(Nil))
  }

  test("combinations: abba (8pts)") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assertEquals(combinations(abba).toSet, abbacomb.toSet)
  }


  test("small sentence anagrams: eat") {
    val sentence = List("eat")
    assertEquals(sentenceAnagrams(sentence).toSet, List(List("eat"), List("ate"), List("tea"), List("a", "et"), List("et", "a")).toSet)
  }

  test("small sentence anagrams: is Lev") {
    val sentence = List("is", "Lev")
    val anas = List(List("lives"), 
                    List("Elvis"), 
                    List("evils"),
                    List("veils"),
                    List("Levis"),
                    List("is", "Lev"),
                    List("Lev", "is"))
    assertEquals(sentenceAnagrams(sentence).toSet, anas.toSet)
  }

  test("sentence anagrams: Linux rulez (10pts)") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assertEquals(sentenceAnagrams(sentence).toSet, anas.toSet)
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
