library(testthat)
testServer(funcParamServer, {
  session$setInputs(x = 1)
  expect_equal(myreactive(), 2)
  expect_equal(output$txt, "I am 2")
  session$setInputs(x = 2)
  expect_equal(myreactive(), 4)
  expect_equal(output$txt, "I am 4")
})