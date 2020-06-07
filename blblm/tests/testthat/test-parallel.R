test_that("parr.blbglm works", {
  expect_equal(typeof(parr.blbglm(Survived~Class+Sex, binomial, data.frame(Titanic),100, 2))
                      , c("list"))
})
