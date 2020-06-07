test_that("confints.blb works", {
  l<-blblm(wt~mpg,mtcars, 1000)
  expect_equal(typeof(confints.blb(l)), c("double"))
})

