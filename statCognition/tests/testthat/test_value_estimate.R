context("Test value estimate")

## .form_block is correct

test_that(".form_block works", {
  contribution_l <- list(contribution(seq(1,10,3), 1:4),
                         contribution(seq(3,15,5), 6:8))
  res <- .form_block(contribution_l)

  expect_true(all(res == c(-Inf, 1, 3, 4, 7, 8, 10, 13)))
})

###################

## .update_hash is correct

test_that(".update_hash works", {
  contribution_l1 <- list(contribution(seq(1,10,3), 1:4),
                          contribution(seq(3,15,5), 6:8))
  contribution_l2 <- list(contribution(seq(11,20,3), 1:4),
                          contribution(seq(13,25,5), 6:8))
  contribution_ll <- list(contribution_l1, contribution_l2)

  block_list <- lapply(contribution_ll, .form_block)

  vec <- c(4,4)
  h <- hash::hash()

  tmp <- .update_hash(h, contribution_ll, block_list, vec)

  expect_true(hash::keys(h) == "4-4")
  expect_true(all(names(h[["4-4"]]) == c("value", "action")))
})

###############

## value_estimate is correct

test_that("value_estimate works", {
  contribution_l1 <- list(contribution(seq(1,10,3), 1:4),
                          contribution(seq(3,15,5), 6:8))
  contribution_l2 <- list(contribution(seq(11,20,3), 1:4),
                          contribution(seq(13,25,5), 6:8))
  contribution_ll <- list(contribution_l1, contribution_l2)

  res <- value_estimate(contribution_ll)

  expect_true(class(res) == "value")
  expect_true(is_valid(res))
})
