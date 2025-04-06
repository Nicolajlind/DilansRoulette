testthat::test_that("Dilansmenu loads as expected", {
  testthat::expect_no_error(dilans_menu())

  menu <- dilans_menu()
  elementer <- nrow(menu)
  numre <- menu$nummer |> unique() |> length()
  testthat::expect_equal(elementer, numre)
})
