test_that("coeff.blb", {
  l<-blblm(wt~mpg,mtcars, 1000)
  expect_equal(typeof(coeff.blb(l)), c("double"))
})
