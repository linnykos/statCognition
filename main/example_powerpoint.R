## script to be shown in powerpoint
set.seed(10)
library(statCognition)
data(dat)
action_ll <- list(list(RC_none, RC_linear_regression,
                       RC_pairing_difference),
                  list(SS_none, SS_cook, SS_neighborhood),
                  list(PD_pearson, PD_kendall, PD_energy))
state_ll <- list(list(state_pheno_MI, state_pheno_residual),
                 list(state_variance, state_interpoint),
                 list(state_samples, state_linearity))

syn_init <- synthetic_initializer()
syn_dat <- lapply(1:20, function(x){
  synthetic_generator(dat, syn_init)})
seed_vec <- sapply(syn_dat, get_seed)

cog_init <- stat_cognition_initializer(action_ll,
              state_ll, syn_init)
res_cog <- stat_cognition(dat, cog_init, seed_vec)
result <- evaluate(res_cog, dat)
result


## script to be shown in appendix
library(statCognition)
dat <- statCognition::dat
compute_color <- function(x){
  sapply(1:nrow(x$pheno), function(y){
    rgb(min(max((x$pheno[y,1])/107,0),1), 0, min(max((107-x$pheno[y,1])/107,0), 1))
  })
}

plot(dat$mat[,1:2], xlab = "", ylab = "", pch = 16, yaxt = "n", xaxt = "n", asp = T,
     col = compute_color(dat))

action_ll <- vector("list", 3)
action_ll[[1]] <- list(RC_none = RC_none, RC_linear_regression = RC_linear_regression,
                       RC_pairing_difference = RC_pairing_difference)
action_ll[[2]] <- list(SS_none = SS_none, SS_cook = SS_cook,
                       SS_neighborhood = SS_neighborhood)
action_ll[[3]] <- list(PD_pearson = PD_pearson, PD_kendall = PD_kendall,
                       PD_energy = PD_energy)
names(action_ll) <- c("Remove_confounder", "Sample_selection", "Pairwise_dependency")

state_ll <- vector("list", 3)
state_ll[[1]] <- list(state_pheno_MI = state_pheno_MI,
                      state_pheno_residual = state_pheno_residual)
state_ll[[2]] <- list(state_variance = state_variance,
                      state_interpoint = state_interpoint)
state_ll[[3]] <- list(state_samples = state_samples, state_linearity = state_linearity)
names(state_ll) <- c("Phenotype", "Outlier", "Dependency")

syn_init <- synthetic_initializer(lambda = 4)
syn_list <- vector("list", 20)
seed_vec <- rep(0, 20)

par(mfrow = c(4,5), mar = rep(0.1, 4))
for(i in 1:20){
  set.seed(i)
  res <- synthetic_generator(dat, syn_init)
  plot(res$mat[,1:2], xlab = "", ylab = "", pch = 16, yaxt = "n", xaxt = "n", asp = T,
       col = compute_color(res))

  syn_list[[i]] <- res
  seed_vec[i] <- get_seed(res)
}

response_vec <- c(1,2,1,   2,1,3,  1,1,1,   2,1,3,  1,1,1,
                  1,3,3,   2,1,1,  1,1,1,   3,2,1,  1,2,1,
                  1,1,3,   1,1,1,  3,1,1,   1,1,1,  3,2,1,
                  1,1,1,   1,3,3,  2,3,3,   3,1,1,  1,1,3)

cog_init <- stat_cognition_initializer(action_ll = action_ll,
                                       state_ll = state_ll,
                                       generator_init = syn_init)

res_cog <- stat_cognition(dat, cog_init, seed_vec, response_vec, store = T)

result <- evaluate(res_cog, dat)
result

## make some plots
pdf("../main/example1.pdf", height = 1, width = 1.5)
par(mar = rep(0.1,4))
res <- .synthetic_generator_seed(dat, syn_init, seed = 266)
plot(res$mat[,1:2], xlab = "", ylab = "", pch = 16, yaxt = "n", xaxt = "n", asp = T,
     col = compute_color(res))
graphics.off()

pdf("../main/example2.pdf", height = 1, width = 1.5)
par(mar = rep(0.1,4))
res <- .synthetic_generator_seed(dat, syn_init, seed = 266)
res <- RC_none(res)
xlim <- range(res$mat[,1]); ylim <- range(res$mat[,2])
plot(res$mat[,1:2], xlab = "", ylab = "", pch = 16, yaxt = "n", xaxt = "n",
     xlim = xlim, ylim = ylim, asp = T)
graphics.off()

pdf("../main/example3.pdf", height = 1, width = 1.5)
par(mar = rep(0.1,4))
res <- .synthetic_generator_seed(dat, syn_init, seed = 266)
res <- RC_none(res)
res <- SS_cook(res)
plot(res$mat[,1:2], xlab = "", ylab = "", pch = 16, yaxt = "n", xaxt = "n",
     xlim = xlim, ylim = ylim, asp = T)
graphics.off()

pdf("../main/example4.pdf", height = 1, width = 1.5)
par(mar = rep(0.1,4))
res <- RC_linear_regression(dat)
xlim <- range(res$mat[,1]); ylim <- range(res$mat[,2])
plot(res$mat[,1:2], xlab = "", ylab = "", pch = 16, yaxt = "n", xaxt = "n",
     xlim = xlim, ylim = ylim, asp = T)
graphics.off()

pdf("../main/example5.pdf", height = 1, width = 1.5)
par(mar = rep(0.1,4))
res <- RC_linear_regression(dat)
res <- SS_none(dat)
xlim <- range(res$mat[,1]); ylim <- range(res$mat[,2])
plot(res$mat[,1:2], xlab = "", ylab = "", pch = 16, yaxt = "n", xaxt = "n",
     xlim = xlim, ylim = ylim, asp = T)
graphics.off()
