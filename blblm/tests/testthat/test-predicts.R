test_that("predicts.blb works", {
  l<-blblm(wt~mpg,mtcars, 1000)
  expect_equal(typeof(predicts.blb(l, data.frame(wt = c(2.5, 3), mpg=c(20,30)), confidence=TRUE)), c("double"))
})
