testthat::test_that("Dilansmenu loads as expected", {
  testthat::expect_no_error(dilans_menu())

  menu <- dilans_menu()
  expected_cols <- c("nummer", "kategori", "navn", "beskrivelse")
  testthat::expect_named(menu, expected_cols)

  elementer <- nrow(menu)
  numre <- menu$nummer |> unique() |> length()
  testthat::expect_equal(elementer, numre)
})
