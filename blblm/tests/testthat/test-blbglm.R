test_that("blbglm works", {
  expect_equal(typeof(blbglm(Survived~Class+Sex, binomial, data.frame(Titanic),100))
               , c("list"))
})
