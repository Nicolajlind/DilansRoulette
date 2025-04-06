testthat::test_that("menu_desc works as expected", {
  testthat::expect_equal(menu_desc("50"), "NO: 50: DURUM GRÆSK BØF")
  testthat::expect_equal(menu_desc("50.A"), "NO: 50.A: DURUM FALAFEL")
  testthat::expect_equal(menu_desc("5"), "NO: 5: MARGHERITA - tomatsauce og ost")
})
