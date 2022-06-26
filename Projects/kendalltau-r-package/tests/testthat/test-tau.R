test_that("tau() equals cor.test(x, y, method = `kendall`)", {
  a = c(1,2,3,4,5,6,7)
  b = c(2,1,4,3,6,7,8)
  corr = as.numeric(cor(a, b, method = 'kendall'))

  expect_equal(tau(a,b), corr)
})
