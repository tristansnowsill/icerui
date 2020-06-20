context("Extracting comparisons from BCEA::bcea() objects")

test_that("incremental effects and costs are extracted with indices", {
  comparison <- extract_comparison.bcea(ex_simple, 1)
  expect_s3_class(comparison, "single_comparison")
  expect_length(comparison$delta.e, ex_simple$n.sim)
  expect_equal(comparison$delta.e, ex_simple$delta.e)
  expect_equal(comparison$delta.c, ex_simple$delta.c)

  comparison <- extract_comparison.bcea(ex_smoking, c(3, 1))
  expect_s3_class(comparison, "multiple_comparison")
  expect_length(comparison, 2)
  expect_length(comparison[[1]]$delta.e, ex_smoking$n.sim)
  expect_equal(comparison[[1]]$delta.e, ex_smoking$delta.e[, 3])
  expect_equal(comparison[[2]]$delta.e, ex_smoking$delta.e[, 1])
})

test_that("incremental effects and costs are extracted with names", {
  comparison <- extract_comparison.bcea(ex_simple, "Control")
  expect_s3_class(comparison, "single_comparison")
  expect_length(comparison$delta.e, ex_simple$n.sim)
  expect_equal(comparison$delta.e, ex_simple$delta.e)
  expect_equal(comparison$delta.c, ex_simple$delta.c)

  comparison <- extract_comparison.bcea(ex_smoking, c("Self-help", "No treatment"))
  expect_s3_class(comparison, "multiple_comparison")
  expect_length(comparison, 2)
  expect_length(comparison[[1]]$delta.e, ex_smoking$n.sim)
  expect_equal(comparison[[1]]$delta.e, ex_smoking$delta.e[, 2])
  expect_equal(comparison[[2]]$delta.e, ex_smoking$delta.e[, 1])
})

test_that("incremental effects and costs are extracted with formulae", {
  comparison <- extract_comparison.bcea(ex_simple, Control ~ Intervention)
  expect_s3_class(comparison, "single_comparison")
  expect_length(comparison$delta.e, ex_simple$n.sim)
  expect_equal(comparison$delta.e, -ex_simple$delta.e)
  expect_equal(comparison$delta.c, -ex_simple$delta.c)

  comparison <- extract_comparison.bcea(ex_smoking, `Group counselling` ~ list(`Self-help`, `No treatment`))
  expect_s3_class(comparison, "multiple_comparison")
  expect_length(comparison, 2)
  expect_length(comparison[[1]]$delta.e, ex_smoking$n.sim)
  expect_equal(comparison[[1]]$delta.e, ex_smoking$delta.e[, 2])
  expect_equal(comparison[[2]]$delta.e, ex_smoking$delta.e[, 1])
})
