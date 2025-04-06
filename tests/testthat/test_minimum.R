testthat::test_that("Minimum works as expected", {
  df1 <- data.frame(ID = numeric())
  testthat::expect_null(minimum(df1$ID))

  df2 <- data.frame(ID = c(2,1,3))
  testthat::expect_equal(minimum(df2$ID), 1)

  df3 <- data.frame(ID = c(NA,11,3))
  testthat::expect_equal(minimum(df3$ID), 3)
})
