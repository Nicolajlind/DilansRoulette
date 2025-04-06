testthat::test_that("Pick next works as expected", {
  gameinfo1 <- structure(
    list(
      ID = 1:3,
      person = c("SA", "Noah", "Ernst"),
      forsog = c(0, 0, 0),
      valgt = c("", "", "")
      ),
    row.names = c(NA, -3L), class = "data.frame")

  testthat::expect_equal(pick_next(1, gameinfo1), 2)
  testthat::expect_equal(pick_next(2, gameinfo1), 3)
  testthat::expect_equal(pick_next(3, gameinfo1), 1)

  gameinfo2 <- structure(
    list(
      ID = 1:3,
      person = c("SA", "Noah", "Ernst"),
      forsog = c(1, 0, 0),
      valgt = c("NO: 49: DURUM KYLLING - NA", "", "")
    ),
    row.names = c(NA, -3L), class = "data.frame")
  testthat::expect_equal(pick_next(1, gameinfo2), 2)
  testthat::expect_equal(pick_next(2, gameinfo2), 3)
  testthat::expect_equal(pick_next(3, gameinfo2), 2)

  gameinfo3 <- structure(
    list(
      ID = 1:3,
      person = c("SA", "Noah", "Ernst"),
      forsog = c(1, 1, 1),
      valgt = c("NO: 49: DURUM KYLLING - NA", "NO: 49: DURUM KYLLING - NA", "NO: 49: DURUM KYLLING - NA")
    ),
    row.names = c(NA, -3L), class = "data.frame")
  testthat::expect_null(pick_next(3, gameinfo3))

})
