

test_that("Total payments exceeds principal with interest", {
  my_monthly_payment <- find_fixedMortgagePayment(P = 10,
                                                  i = 0.1,
                                                  T = 1,
                                                  k = 12)
  total_payments <- my_monthly_payment*12*1
  expect_true(total_payments > 10)
  # expect_true(total_payments > 10*(1+0.1)) # verified with online check 
})