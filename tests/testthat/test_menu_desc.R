testthat::test_that("menu_desc works as expected", {
  testthat::expect_equal(menu_desc("50"), "NO: 50: Durum Ruller - GRÆSK BØF")
  testthat::expect_equal(menu_desc("50.A"), "NO: 50.A: Durum Ruller - FALAFEL")
  testthat::expect_equal(menu_desc("5"), "NO: 5: Pizza - MARGHERITA - tomatsauce og ost")
})
