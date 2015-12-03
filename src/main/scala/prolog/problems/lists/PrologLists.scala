package prolog.problems.lists

import scala.annotation.tailrec

object PrologLists extends App {
  // Find the last element of a list
  def p1[A](list: List[A]): Option[A] = list.lastOption

  // Find the last but one element of a list
  def p2[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case _ => list.reverse.tail.headOption
  }

  // Find the K'th element of a list
  def p3a[A](list: List[A], index: Int): Option[A] = index match {
    case i if i > 0 && i < list.length + 1 && list.nonEmpty => Some(list(index - 1))
    case _ => None
  }

  // Find the K'th element of a list (using zipWithIndex)
  def p3b[A](list: List[A], index: Int): Option[A] = list.zipWithIndex.find(_._2 + 1 == index).map(_._1)

  // Find the number of elements of a list
  def p4(list: List[_]): Int = list.length

  // Reverse a list
  def p5[A](list: List[A]): List[A] = list.reverse

  // Find out whether a list is a palindrome
  @tailrec
  def p6(list: List[_]): Boolean = {
    list match {
      case Nil => true
      case _ :: Nil => true
      case head :: tail if head != tail.last => false
      case head :: tail if head == tail.last => p6(list.tail.init)
    }
  }

  // Flatten a nested list structure
  def p7(list: List[_]): List[Any] = list flatMap {
    case l: List[_] => p7(l)
    case v => List(v)
  }

  // Eliminate consecutive duplicates of list elements
  def p8[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case _ => list.init.foldRight(List(list.last))((a, b) => if (b.head == a) b else a :: b)
  }

  // Pack consecutive duplicates of list elements into sublists
  def p9[A](list: List[A]): List[List[A]] = list match {
    case Nil => Nil
    case _ => list.init.foldRight(List(List(list.last))) { (a, b) =>
      if (b.head.head == a) (a :: b.head) :: b.tail
      else List(a) :: b
    }
  }

  // Run-length encoding of a list
  def p10(list: List[_]): List[List[_]] = {
    p9(list).map { v =>
      List(v.length, v.head)
    }
  }

  // Modified run-length encoding
  def p11(list: List[_]): List[Any] = {
    p9(list).map { v =>
      if (v.lengthCompare(1) == 0) v.head
      else List(v.length, v.head)
    }
  }

  // Decode a run-length encoded list
  // TODO: flatten result list
  def p12(list: List[_]): List[_] = {
    list.flatMap {
      case length :: value => length match {
        case x: Int => List.fill(x)(value)
        case _ => Nil
      }
      case v => List(v)
    }
  }

  // Run-length encoding of a list (direct solution)
  def p13[A](list: List[A]) =
    if (list.isEmpty) list
    else {
      list.init.foldRight(List((1, list.last))) { (a, b) =>
        if (b.head._2 == a) (b.head._1 + 1, a) :: b.tail
        else (1, a) :: b
      }.map {
        case (1, s) => s
        case c => c
      }
    }

  // Duplicate the elements of a list
  def p14(list: List[_]): List[_] = list.flatMap(List.fill(2)(_))

  // Duplicate the elements of a list a given number of times
  def p15(list: List[_], n: Int): List[_] = list.flatMap(List.fill(n)(_))

  // Drop every N'th element from a list
  def p16(list: List[_], n: Int): List[_] = list.zipWithIndex.filter(_._2 + 1 % n == 0)

  // Split a list into two parts; the length of the first part is given
  def p17(list: List[_], n: Int): (List[_], List[_]) = (list.take(n), list.drop(n))

  // Extract a slice from a list
  def p18(list: List[_], a: Int, b: Int): List[_] = list.slice(a - 1, b)

  // Rotate a list N places to the left
  // TODO: doesn't work with negative n
  def p19(list: List[_], n: Int) = list.drop(n) ::: list.take(n)

  // Remove the K'th element from a list
  def p20[A](list: List[A], n: Int): (A, List[A]) = (list(n - 1), list.take(n - 1) ::: list.drop(n))

  // Insert an element at a given position into a list
  def p21[A](list: List[A], elem: A, pos: Int): List[A] = list.take(pos - 1) ::: elem :: list.drop(pos - 1)

  // Create a list containing all integers within a given range
  def p22(a: Int, b: Int): List[Int] =
    if (a <= b) (a to b).toList
    else (b to a).toList.reverse

  // Extract a given number of randomly selected elements from a list
  def p23[A](list: List[A], n: Int): List[A] = {

    def removeAt[B](n: Int, list: List[B]): (B, List[B]) = {
      val (head, rm :: tail) = list.splitAt(n)
      (rm, head ::: tail)
    }

    import scala.util.Random
    val random = new Random()
    if (n > 0) {
      val (removed, tail) = removeAt(random.nextInt(list.length), list)
      removed :: p23(tail, n - 1)
    } else {
      Nil
    }
  }

  // Lotto: Draw N different random numbers from the set 1..M
  def p24(n: Int, m: Int): List[Int] = {
    import scala.util.Random
    val r = new Random
    Stream.continually(1 + r.nextInt(m)).distinct.take(n).toList
  }

  // Generate a random permutation of the elements of a list
  def p25[A](list: List[A]): List[A] = {
    import scala.util.Random
    val r = new Random
    val perm = list.permutations.toList
    perm(r.nextInt(perm.length))
  }

  // Generate the combinations of K distinct objects chosen from the N elements of a list
  def p26[A](list: List[A], n: Int)(implicit ev1: A => Ordered[A]): List[List[A]] = {
    list.permutations.toList.map(_.take(n).sorted).distinct
  }

  // Group the elements of a set into disjoint subsets
  def p27[A](list: List[A], group: List[Int])(implicit ev1: A => Ordered[A]): List[List[List[A]]] = {
    val differs = (l1: List[A], l2: List[A]) => l1.diff(l2) == l1
    val perm = list.permutations.toList
    val grouped = (i: Int) => perm.map(_.take(group(i)).sorted).toList.distinct

    val g1 = grouped(0)
    val g2 = grouped(1)
    val g3 = grouped(2)

    g1.flatMap { g1e =>
      g2.filter(differs(_, g1e)).map(g1e ::: _).flatMap { g2e =>
        g3.filter(differs(_, g2e)).map(g2e ::: _)
      }
    }.map { v =>
      List(
        v.slice(0, group.head),
        v.slice(group.head, group.head + group(1)),
        v.slice(group.head + group(1), group.head + group(1) + group(2)))
    }
  }

  // Sorting a list of lists according to length of sublists
  def p28a[A](list: List[List[A]]): List[List[A]] = list.sortWith(_.length < _.length)

  // Sorting a list of lists according to length frequency of sublists
  def p28b[A](list: List[List[_]]): List[List[_]] = {
    implicit def tupleOrdering[B](implicit ev1: B => (List[_], Int)): Ordering[B] = Ordering.by(_._2)
    val lengthMap = list.view.map(_.length).groupBy(x => x).map(p => (p._1, p._2.length))
    list.view.map(x => (x, lengthMap.get(x.length).get)).sorted.map(_._1).force.toList
  }
}
