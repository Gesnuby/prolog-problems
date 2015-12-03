package prolog.problems.lists

trait TestData {
  val nonEmptyList = List(1, 2, 3, 4)
  val emptyList = List.empty[T forSome {type T}]
  val oneElementList = List(1)
  val fiveElementsList = List(1, 2, 3, 4, 5)

  val listWithDups = List(1, 1, 2, 3, 4, 4, 1, 2, 3, 3, 5)
  val lengthPackedList = List(
    List(2, 1), List(1, 2), List(1, 3),
    List(2, 4), List(1, 1), List(1, 2),
    List(2, 3), List(1, 5)
  )
}
