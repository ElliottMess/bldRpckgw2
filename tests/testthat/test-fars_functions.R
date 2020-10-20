test_that("make_filename works", {
  expect_equal(make_filename(2008), "accident_2008.csv.bz2")
})
