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

  plot(obj, type = "contribution_surface")

  pdf("Rplots.pdf", height = 6, width = 8)
  .plot_value_contribution_surface(obj)
  graphics.off(); file.remove("Rplots.pdf")
})
