context("Test plotting value")

## .plot_value_contribution is correct

test_that(".plot_value_contribution works",{
  contribution_l1 <- list(action1 = contribution(seq(1,10,3), c(10,1:3)),
                          action2 = contribution(seq(3,15,5), 6:8))
  contribution_l2 <- list(action1 = contribution(seq(11,20,3), c(10,1:3)),
                          action2 = contribution(seq(13,25,5), 6:8))
  contribution_ll <- list(contribution_l1, contribution_l2)
  names(contribution_ll) <- c("Dim1", "Dim2")

  obj <- value_estimate(contribution_ll, store = T)

  pdf("Rplots.pdf", height = 6, width = 8)
  plot(obj)
  graphics.off(); file.remove("Rplots.pdf")
})

test_that(".plot_value_contribution works with parameters",{
  contribution_l1 <- list(action1 = contribution(seq(1,10,3), c(10,1:3)),
                          action2 = contribution(seq(3,15,5), 6:8))
  contribution_l2 <- list(action1 = contribution(seq(11,20,3), c(10,1:3)),
                          action2 = contribution(seq(13,25,5), 6:8))
  contribution_ll <- list(contribution_l1, contribution_l2)
  names(contribution_ll) <- c("Dim1", "Dim2")

  obj <- value_estimate(contribution_ll, store = T)

  pdf("Rplots.pdf", height = 6, width = 8)
  plot(obj, line_lwd = 10)
  graphics.off(); file.remove("Rplots.pdf")
})
