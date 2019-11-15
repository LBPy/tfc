test_that("D computes the correct value",{
  y = rep(20,20)
  n=length(y)
  l=5
  g <-function(y,l) n* integrate(function(t) sapply(t,function(t) abs(sum(exp(t*1i*y))/n - exp(-abs(t)) )^2 *exp(-l *abs(t))) , -Inf, Inf)$value
  expect_equal(D_Henze(y,l), g(y,l), tolerance = .002)
})

