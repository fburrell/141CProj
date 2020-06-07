test_that("se.blb", {
  l<-blblm(wt~mpg,mtcars, 1000)
  expect_equal(typeof(se.blb(l)), c("double"))
})
