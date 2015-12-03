import org.scalatest.{FunSpec, Matchers}
import prolog.problems.lists.PrologLists._
import prolog.problems.lists.TestData

class PrologListsTest extends FunSpec with Matchers with TestData {

  describe("Function p1") {
    it("should return last element of non-empty list") {
      p1(nonEmptyList) shouldBe Some(4)
    }
    it("should return None if initial list is empty") {
      p1(emptyList) shouldBe None
    }
  }

  describe("Function p2") {
    it("should return last but one element of non-empty list with more than 1 element") {
      p2(nonEmptyList) shouldBe Some(3)
    }
    it("should return None if list has only one element") {
      p2(oneElementList) shouldBe None
    }
    it("should return None if list is empty") {
      p2(emptyList) shouldBe None
    }
  }

  describe("Function p3a") {
    it("should return k'th element of non-empty list") {
      p3a(nonEmptyList, 4) shouldBe Some(4)
    }
    it("should return None is list is empty") {
      p3a(emptyList, 2) shouldBe None
    }
    it("should return None if there isn't k-th element in a list") {
      p3a(nonEmptyList, 5) shouldBe None
    }
  }

  describe("Function p3b") {
    it("should return k'th element of non-empty list") {
      p3b(nonEmptyList, 4) shouldBe Some(4)
    }
    it("should return None is list is empty") {
      p3b(emptyList, 2) shouldBe None
    }
    it("should return None if there isn't k-th element in a list") {
      p3b(nonEmptyList, 5) shouldBe None
    }
  }

  describe("Function p4") {
    it("should return number of elements in a list") {
      p4(fiveElementsList) shouldBe 5
    }
    it("should return 0 if list is empty") {
      p4(emptyList) shouldBe 0
    }
  }

  describe("Function p5") {
    it("should return reversed list") {
      p5(nonEmptyList) shouldBe nonEmptyList.reverse
    }
    it("should return empty list if initial list is empty") {
      p5(emptyList) shouldBe Nil
    }
  }

  describe("Function p6") {
    it("should return true if list is palindrome") {
      p6(List("k", "a", "b", "a", "k")) shouldBe true
    }
    it("should return false if list is not palindrome") {
      p6(List("k", "a", "b", "a")) shouldBe false
    }
  }

  describe("Function p7") {
    it("should flatten nested lists") {
      p7(List(0, List(1, 2), List(List(3), List(4)))) shouldBe List(0, 1, 2, 3, 4)
    }
    it("should return initial list if it hasn't nested lists") {
      p7(fiveElementsList) shouldBe fiveElementsList
    }
  }

  describe("Function p8") {
    it("should eliminate consecutive duplicates of list elements") {
      p8(listWithDups) shouldBe List(1, 2, 3, 4, 1, 2, 3, 5)
    }
    it("should return empty list if initial list is empty") {
      p8(emptyList) shouldBe Nil
    }
    it("should return initial list if it hasn't duplicates") {
      p8(fiveElementsList) shouldBe fiveElementsList
    }
  }
  describe("Function p9") {
    it("should pack consecutive duplicates of list elements into sublists") {
      p9(listWithDups) shouldBe
        List(
          List(1, 1), List(2), List(3),
          List(4, 4), List(1), List(2),
          List(3, 3), List(5)
        )
    }
    it("should return empty list if initial list is empty") {
      p9(emptyList) shouldBe Nil
    }
  }

  describe("Function p10") {
    it("should return run-length encoded list") {
      p10(listWithDups) shouldBe lengthPackedList
    }
    it("should return empty list if initial list is empty") {
      p10(emptyList) shouldBe Nil
    }
  }

  describe("Function p11") {
    it("should return run-length encoded list in which elements that has no duplicates are copied to result list") {
      p11(listWithDups) shouldBe
        List(
          List(2, 1), 2, 3,
          List(2, 4), 1, 2,
          List(2, 3), 5
        )
    }
    it("should return empty list if initial list is empty") {
      p11(emptyList) shouldBe Nil
    }
  }

  describe("Function p12") {
    it("should decode run-length encoded list") {
      p12(lengthPackedList) shouldBe listWithDups
    }
  }

  describe("Function p13") {
    it("should return run-length encoded list (direct solution)") {
      p13(listWithDups) shouldBe List((2, 1), 2, 3, (2, 4), 1, 2, (2, 3), 5)
    }
    it("should return empty list if initial list is empty") {
      p13(emptyList) shouldBe Nil
    }
  }

  describe("Function p14") {
    it("should duplicate the elements of a list") {
      p14(listWithDups) shouldBe List(1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 1, 1, 2, 2, 3, 3, 3, 3, 5, 5)
    }
    it("should return empty list if initial list is empty") {
      p14(emptyList) shouldBe Nil
    }
  }

  describe("Function p15") {
    it("should duplicate the elements of a list a given number of times") {
      p15(listWithDups, 3) shouldBe List(1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 4, 4, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 5, 5, 5)
    }
    it("should return empty list if initial list is empty") {
      p15(emptyList, 5) shouldBe Nil
    }
  }

  describe("Function p16") {
    it("should drop every n-th from a list") {
      p16(fiveElementsList, 2) shouldBe List(1, 3, 5)
    }
    it("should return empty list if n = 1") {
      p16(fiveElementsList, 1) shouldBe Nil
    }
  }

  describe("Function p17") {
    it("should split a list into two parts; the length of the first part is given") {
      p17(fiveElementsList, 3) shouldBe(List(1, 2, 3), List(4, 5))
    }
  }

  describe("Function p18") {
    it("should extract a slice from a list") {
      p18(fiveElementsList, 3, 4) shouldBe List(3, 4)
    }
  }

  describe("Function p19") {
    it("should rotate a list n places to the left") {
      p19(fiveElementsList, 2) shouldBe List(3, 4, 5, 1, 2)
    }

    it("should rotate a list n places to the left even if n is negative") {
      p19(fiveElementsList, 2) shouldBe List(4, 5, 1, 2, 3)
    }
  }

  describe("Function p20") {
    it("should remove k-th element from a list") {
      val result = p20(fiveElementsList, 3)
      result._1 shouldBe 3
      result._2 shouldBe List(1, 2, 4, 5)
    }
  }

  describe("Function p21") {
    it("should insert an element at a given position into a list") {
      p21(fiveElementsList, 666, 3) shouldBe List(1, 2, 666, 3, 4, 5)
    }
  }

  describe("Function p22") {
    it("should create a list containing all integers within a given range") {
      p22(3, 7) shouldBe List(3, 4, 5, 6, 7)
    }

    it("should create a list containing all integers within a given range (first integer > second integer)") {
      p22(7, 3) shouldBe List(7, 6, 5, 4, 3)
    }
  }

  describe("Function p26") {
    it("should generate the combinations of K distinct objects chosen from the N elements of a list") {
      p26(List(1, 2, 3), 2) shouldBe
        List(
          List(1, 2), List(1, 3), List(2, 3)
        )
    }
  }

  describe("Function p27") {
    it("should group the elements of a set into 3 disjoint subsets") {
      p27(List("a", "b", "c", "d"), List(1, 2, 1)) shouldBe
        List(
          List(List("a"), List("b", "c"), List("d")),
          List(List("a"), List("b", "d"), List("c")),
          List(List("a"), List("c", "d"), List("b")),
          List(List("b"), List("a", "c"), List("d")),
          List(List("b"), List("a", "d"), List("c")),
          List(List("b"), List("c", "d"), List("a")),
          List(List("c"), List("a", "b"), List("d")),
          List(List("c"), List("a", "d"), List("b")),
          List(List("c"), List("b", "d"), List("a")),
          List(List("d"), List("a", "b"), List("c")),
          List(List("d"), List("a", "c"), List("b")),
          List(List("d"), List("b", "c"), List("a"))
        )
    }
  }

  describe("Function p28a") {
    it("should sort a list of lists according to length of sublists") {
      p28a(List(List(1, 2, 3), List(4, 5), List(6), List(7, 8), List(9))) shouldBe
        List(
          List(6), List(9), List(4, 5), List(7, 8), List(1, 2, 3)
        )
    }
  }

  describe("Function p28b") {
    it("should sort a list of lists according to length frequency of sublists") {
      p28b(List(
        List("a", "b", "c"), List("d", "e"), List("f", "g", "h"),
        List("d", "e"), List("i", "j", "k", "l"), List("m", "n"), List("o")
      )) shouldBe List(
        List("i", "j", "k", "l"), List("o"), List("a", "b", "c"),
        List("f", "g", "h"), List("d", "e"), List("d", "e"), List("m", "n")
      )
    }
  }
}
