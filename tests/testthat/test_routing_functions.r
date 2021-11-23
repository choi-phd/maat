test_that(
  "boundGrade() works",
  {
    # upper bound works
    expect_equal(boundGrade("G7", "G5", 0, 2), "G7")
    expect_equal(boundGrade("G8", "G5", 0, 2), "G7")

    # lower bound works
    expect_equal(boundGrade("G3", "G5", 2, 0), "G3")
    expect_equal(boundGrade("G2", "G5", 2, 0), "G3")

    # should error
    expect_error(boundGrade("G5", "5", 2, 2))
    expect_error(boundGrade("5", "G5", 2, 2))
    expect_error(boundGrade("G5", "GG5", 2, 2))
    expect_error(boundGrade("GG5", "G5", 2, 2))

    # should error
    expect_error(boundGrade("G5", "G5", -1, 0))
    expect_error(boundGrade("G5", "G5", 0, -1))
  }
)
