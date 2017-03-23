context("Test correlation graph")

## CG_pearson is correct

test_that("CG_pearson works", {
  set.seed(10)
  dat <- huge::huge.generator(n = 50, d = 12, graph = "hub", g = 4, verbose = F)$data
  res <- CG_pearson(dat, threshold = 0.3)

  expect_true(is.matrix(res))
  expect_true(all(dim(res) == 12))
  expect_true(all(unique(as.numeric(res)) %in% c(0,1)))
})

##################

## CG_kendalls_tau is correct

test_that("CG_kendalls_tau works", {
  set.seed(10)
  dat <- huge::huge.generator(n = 50, d = 12, graph = "hub", g = 4, verbose = F)$data
  res <- CG_kendalls_tau(dat, threshold = 0.3)

  expect_true(is.matrix(res))
  expect_true(all(dim(res) == 12))
  expect_true(all(unique(as.numeric(res)) %in% c(0,1)))
})

##################

## CG_distance_correlation is correct

test_that("CG_distance_correlation works", {
  set.seed(10)
  dat <- huge::huge.generator(n = 50, d = 12, graph = "hub", g = 4, verbose = F)$data
  res <- CG_distance_correlation(dat, threshold = 0.3)

  expect_true(is.matrix(res))
  expect_true(all(dim(res) == 12))
  expect_true(all(unique(as.numeric(res)) %in% c(0,1)))
})

##################

## CG_discrete_mutual_information is correct

test_that("CG_discrete_mutual_information works", {
  set.seed(10)
  dat <- huge::huge.generator(n = 50, d = 12, graph = "hub", g = 4, verbose = F)$data
  res <- CG_discrete_mutual_information(dat, threshold = 0.1)

  expect_true(is.matrix(res))
  expect_true(all(dim(res) == 12))
  expect_true(all(unique(as.numeric(res)) %in% c(0,1)))
})
