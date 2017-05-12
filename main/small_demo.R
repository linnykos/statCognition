load("../main/assets/brainspan_PFC35_top50gene_extract.RData")
load("../main/assets/brainspan_PFC35_phenotype.RData")

mat <- mat[,c(3,6)]
pheno$Days <- 1:nrow(pheno)
pheno <- data.frame(Days = pheno$Days)
dat <- data_object(list(mat = mat, pheno = pheno))

####
#load("tests/assets/demo.Rdata")

par(mfrow = c(5,5), mar = rep(0.1, 4))
compute_color <- function(x){
  sapply(1:nrow(x$pheno), function(y){
    rgb(min(max((x$pheno[y,1])/107,0),1), 0, min(max((107-x$pheno[y,1])/107,0), 1))
  })
}
#plot original data
plot(dat$mat[,1:2], xlab = "", ylab = "", pch = 16, yaxt = "n", xaxt = "n", asp = T,
     col = compute_color(dat))

#plot 20 synthetic datasets
init <- synthetic_initializer(lambda = 4)
for(i in 1:20){
  set.seed(i)
  res <- synthetic_generator(dat, init)
  plot(res$mat[,1:2], xlab = "", ylab = "", pch = 16, yaxt = "n", xaxt = "n", asp = T,
       col = compute_color(res))
  print(get_seed(res))
}

################################
action_ll <- vector("list", 3)
action_ll[[1]] <- list(RC_none = RC_none, RC_linear_regression = RC_linear_regression,
                       RC_pairing_difference = RC_pairing_difference)
action_ll[[2]] <- list(SS_none = SS_none, SS_cook = SS_cook,
                       SS_neighborhood = SS_neighborhood)
action_ll[[3]] <- list(PD_pearson = PD_pearson, PD_kendall = PD_kendall,
                       PD_energy = PD_energy)
names(action_ll) <- c("Remove_confounder", "Sample_selection", "Pairwise_dependency")

## for each dataset, plot all 3+9 plots and list all 27 outputs
for(i in 1:20){
  par(mfrow = c(4, 3), mar = rep(0.1, 4))
  set.seed(i)
  res <- synthetic_generator(dat, init)

  #do the phenotype first
  for(j in 1:3){
    res2 <- action_ll[[1]][[j]](res)
    plot(res2$mat[,1:2], xlab = "", ylab = "", pch = 16, yaxt = "n", xaxt = "n", asp = T)
  }

  #do outlier detection next
  for(j in 1:3){
    tmp_lis <- vector("list", 3)

    for(k in 1:3){
      res2 <- action_ll[[1]][[j]](res)
      tmp_lis[[k]] <- action_ll[[2]][[k]](res2)
    }

    min_vec <- apply(sapply(tmp_lis, function(x){apply(x$mat, 2, min)}), 1, min)
    max_vec <- apply(sapply(tmp_lis, function(x){apply(x$mat, 2, max)}), 1, max)
    xlim <- c(min_vec[1], max_vec[1]); ylim <- c(min_vec[2], max_vec[2])

    for(k in 1:3){
      plot(tmp_lis[[k]]$mat[,1:2], xlab = "", ylab = "", pch = 16, yaxt = "n", xaxt = "n", asp = T,
           xlim = xlim, ylim = ylim)
    }
  }

  for(j in 1:3){
    for(k in 1:3){
      for(l in 1:3){
        res2 <- action_ll[[1]][[j]](res)
        res2 <- action_ll[[2]][[k]](res2)
        print(paste0(i, "--", j, ":", k, ":", l, "- ", action_ll[[3]][[l]](res2)))
      }
    }
  }
}
