test_that("hello world works", {
  out <- capture.output(hello())
  expect_true(out == "[1] \"Hello, world!\"")
})
