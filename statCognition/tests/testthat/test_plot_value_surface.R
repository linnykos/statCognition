context("Test plotting value surface")

## .plot_value_contribution_surface is correct

test_that(".plot_value_contribution_surface works",{
  contribution_l1 <- list(action1 = contribution(seq(1,10,3), c(10,1:3)),
                          action2 = contribution(seq(3,15,5), 6:8))
  contribution_l2 <- list(action1 = contribution(seq(11,20,3), c(10,1:3)),
                          action2 = contribution(seq(13,25,5), 6:8))
  contribution_ll <- list(contribution_l1, contribution_l2)
  names(contribution_ll) <- c("Dim1", "Dim2")

  obj <- value_estimate(contribution_ll, store = T)

  pdf("Rplots.pdf", height = 6, width = 8)
  plot(obj, type = "contribution_surface")
  graphics.off(); file.remove("Rplots.pdf")
})

###################

## .pad_vector is correct

test_that(".pad_vector works", {
  res <- .pad_vector(1:5, tol = .5)
  width <- diff(range(1:5))/20
  expect_true(all(res == c(1-width, 1, 2-.5, 2, 3-.5, 3, 4-.5, 4, 5-.5, 5, 5+width)))
})

test_that(".pad_vector works for a singleton", {
  res <- .pad_vector(1, tol = .5)
  expect_true(all(res == c(0,1,2)))
})
