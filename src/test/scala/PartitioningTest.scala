class PartitioningTest extends munit.FunSuite {

  test("[25, 49]") {
    assertNoDiff(
      Partitioning.of(25, 49).toString,
      "[5 x 5 ; 7 x 7]"
    )
  }

  test("[24, 49]") {
    assertNoDiff(
      Partitioning.of(24, 49).toString,
      "{24} + [5 x 5 ; 7 x 7]"
    )
  }

  test("[25, 50]") {
    assertNoDiff(
      Partitioning.of(25, 50).toString,
      "[5 x 5 ; 7 x 7] + {50}"
    )
  }

  test("[24, 50]") {
    assertNoDiff(
      Partitioning.of(24, 50).toString,
      "{24} + [5 x 5 ; 7 x 7] + {50}"
    )
  }

  test("[26, 49]") {
    assertNoDiff(
      Partitioning.of(26, 49).toString,
      "- {25} + [5 x 5 ; 7 x 7]"
    )
  }

  test("[25, 48]") {
    assertNoDiff(
      Partitioning.of(25, 48).toString,
      "[5 x 5 ; 7 x 7] - {49}"
    )
  }

  test("[26, 48]") {
    assertNoDiff(
      Partitioning.of(26, 48).toString,
      "- {25} + [5 x 5 ; 7 x 7] - {49}"
    )
  }

  test("[26, 50]") {
    assertNoDiff(
      Partitioning.of(26, 50).toString,
      "- {25} + [5 x 5 ; 7 x 7] + {50}"
    )
  }

  test("[24, 48]") {
    assertNoDiff(
      Partitioning.of(24, 48).toString,
      "{24} + [5 x 5 ; 7 x 7] - {49}"
    )
  }

  test("[1, 25]") {
    assertNoDiff(
      Partitioning.of(1, 25).toString,
      "[1 x 1 ; 5 x 5]"
    )
  }

  test("[1, 26]") {
    assertNoDiff(
      Partitioning.of(1, 26).toString,
      "[1 x 1 ; 5 x 5] + {26}"
    )
  }

  test("[1, 24]") {
    assertNoDiff(
      Partitioning.of(1, 24).toString,
      "[1 x 1 ; 5 x 5] - {25}"
    )
  }

  test("[2, 25]") {
    assertNoDiff(
      Partitioning.of(2, 25).toString,
      "- {1} + [1 x 1 ; 5 x 5]"
    )
  }

  test("[3, 25]") {
    assertNoDiff(
      Partitioning.of(3, 25).toString,
      "{3} + [2 x 2 ; 5 x 5]"
    )
  }

  test("[4, 25]") {
    assertNoDiff(
      Partitioning.of(4, 25).toString,
      "[2 x 2 ; 5 x 5]"
    )
  }

  test("[5, 25]") {
    assertNoDiff(
      Partitioning.of(5, 25).toString,
      "- {4} + [2 x 2 ; 5 x 5]"
    )
  }

  test("[6, 25]") {
    assertNoDiff(
      Partitioning.of(6, 25).toString,
      "- [5 ; 4] + [2 x 2 ; 5 x 5]"
    )
  }

  test("[7, 25]") {
    assertNoDiff(
      Partitioning.of(7, 25).toString,
      "[7 ; 8] + [3 x 3 ; 5 x 5]"
    )
  }

  test("[8, 25]") {
    assertNoDiff(
      Partitioning.of(8, 25).toString,
      "{8} + [3 x 3 ; 5 x 5]"
    )
  }

  test("[9, 25]") {
    assertNoDiff(
      Partitioning.of(9, 25).toString,
      "[3 x 3 ; 5 x 5]"
    )
  }

  test("[10, 25]") {
    assertNoDiff(
      Partitioning.of(10, 25).toString,
      "- {9} + [3 x 3 ; 5 x 5]"
    )
  }

  test("[11, 25]") {
    assertNoDiff(
      Partitioning.of(11, 25).toString,
      "- [10 ; 9] + [3 x 3 ; 5 x 5]"
    )
  }

  test("[12, 25]") {
    assertNoDiff(
      Partitioning.of(12, 25).toString,
      "- [11 ; 9] + [3 x 3 ; 5 x 5]"
    )
  }

  test("[13, 25]") {
    assertNoDiff(
      Partitioning.of(13, 25).toString,
      "- [12 ; 9] + [3 x 3 ; 5 x 5]"
    )
  }

  test("[14, 25]") {
    assertNoDiff(
      Partitioning.of(14, 25).toString,
      "- [13 ; 9] + [3 x 3 ; 5 x 5]"
    )
  }

  test("[15, 25]") {
    assertNoDiff(
      Partitioning.of(15, 25).toString,
      "- [14 ; 9] + [3 x 3 ; 5 x 5]"
    )
  }

  test("[16, 25]") {
    assertNoDiff(
      Partitioning.of(16, 25).toString,
      "- [15 ; 9] + [3 x 3 ; 5 x 5]"
    )
  }

  test("[17, 25]") {
    assertNoDiff(
      Partitioning.of(17, 25).toString,
      "[17 ; 24] + {25}"
    )
  }

  test("[18, 25]") {
    assertNoDiff(
      Partitioning.of(18, 25).toString,
      "[18 ; 24] + {25}"
    )
  }

  test("[19, 25]") {
    assertNoDiff(
      Partitioning.of(19, 25).toString,
      "[19 ; 24] + {25}"
    )
  }

  test("[20, 25]") {
    assertNoDiff(
      Partitioning.of(20, 25).toString,
      "[20 ; 24] + {25}"
    )
  }

  test("[21, 25]") {
    assertNoDiff(
      Partitioning.of(21, 25).toString,
      "[21 ; 24] + {25}"
    )
  }

  test("[22, 25]") {
    assertNoDiff(
      Partitioning.of(22, 25).toString,
      "[22 ; 24] + {25}"
    )
  }

  test("[23, 25]") {
    assertNoDiff(
      Partitioning.of(23, 25).toString,
      "[23 ; 24] + {25}"
    )
  }

  test("[24, 25]") {
    assertNoDiff(
      Partitioning.of(24, 25).toString,
      "{24} + {25}"
    )
  }

  test("[25, 25]") {
    assertNoDiff(
      Partitioning.of(25, 25).toString,
      "{25}"
    )
  }

  test("[26, 25]") {
    intercept[Exception] {
      Partitioning.of(26, 25)
    }
  }

  test("[0, 1]") {
    intercept[Exception] {
      Partitioning.of(0, 1)
    }
  }

}
