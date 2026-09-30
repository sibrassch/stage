# DATA SIMULATION AND PARAMETER RECOVERY IMPROVED SWITCH MODEL with fixed alphaPE or fixed pe_threshold
# source for helperfunctions:
source("/Users/sibrass/Documents/STAGE/stage/functions.R")

################################################################################################################################################################
# simulate data (alphaPE = 0.5)
n_sims <- 200

set.seed(123)
true_alpha          <- runif(n_sims, min = 0, max = 1)
true_beta           <- runif(n_sims, min = 0, max = 10)
true_alphaPE        <- rep(0.5, n_sims)
true_pe_threshold   <- runif(n_sims, min = -0.45, max = -0.05)

imprswitch_list <- vector("list", n_sims) # creating a list of 200 dataframes (one for every output of 100 trials)

################################################################################################################################################################
# simulate data (pe_threshold = -0.4)
n_sims <- 200

set.seed(123)
true_alpha          <- runif(n_sims, min = 0, max = 1)
true_beta           <- runif(n_sims, min = 0, max = 10)
true_alphaPE        <- runif(n_sims, min = 0.05, max = 0.45)
true_pe_threshold   <- rep(-0.2, n_sims)

imprswitch_list <- vector("list", n_sims) # creating a list of 200 dataframes (one for every output of 100 trials)

################################################################################################################################################################
# actual loop
for (i in 1:n_sims) {
  imprswitch_list[[i]] <- simulate_impr_switch(
    n_trials             = 400,
    correct_stim         = "green",
    reversal_trial       = seq(25, 375, by = 25),
    p_reward_correct     = 0.90,
    p_reward_incorrect   = 0.10,
    alpha                = true_alpha[i],
    beta                 = true_beta[i],
    alphaPE              = true_alphaPE[i],
    pe_threshold         = true_pe_threshold[i]
  )
}

################################################################################################################################################################
# summary
sum_impr_switch <- data.frame(
  sim              = 1:n_sims,
  alpha            = true_alpha,
  beta             = true_beta,
  alphaPE          = true_alphaPE,
  pe_threshold     = true_pe_threshold,
  prop_correct     = sapply(imprswitch_list, function(d) mean(d$correct)),
  n_switches       = sapply(imprswitch_list, function(d) sum(d$switch_dec))
)

summary(sum_impr_switch$n_switches)

# relation between parameters and performance / switching
par(mfrow = c(2, 2))
plot(sum_impr_switch$alpha, sum_impr_switch$prop_correct,
     xlab = "true alpha", ylab = "proportion correct",
     main = "Learning rate vs performance", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
plot(sum_impr_switch$beta, sum_impr_switch$prop_correct,
     xlab = "true beta", ylab = "proportion correct",
     main = "Inverse temperature vs performance", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")

# for fixed alphaPE
plot(sum_impr_switch$pe_threshold, sum_impr_switch$prop_correct,
     xlab = "true pe_threshold", ylab = "proportion correct",
     main = "PE threshold vs performance", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
plot(sum_impr_switch$pe_threshold, sum_impr_switch$n_switches,
     xlab = "true pe_threshold", ylab = "number of switches",
     main = "PE threshold vs switches", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")

# for fixed pe_threshold
plot(sum_impr_switch$alphaPE, sum_impr_switch$prop_correct,
     xlab = "true alphaPE", ylab = "proportion correct",
     main = "alphaPE vs performance", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
plot(sum_impr_switch$alphaPE, sum_impr_switch$n_switches,
     xlab = "true alphaPE", ylab = "number of switches",
     main = "alphaPE vs switches", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")

################################################################################################################################################################
# fit function: grid for pe_threshold, optim for alpha and beta, alphaPE fixed
fit_grid <- function(d,
                     th_grid = seq(-0.55, -0.01, by = 0.02),
                     aPE_grid = 0.5,
                     ab_starts = list(c(alpha = 0.3, beta = 3),
                                      c(alpha = 0.6, beta = 1.5),
                                      c(alpha = 0.1, beta = 6))) {
  best <- list(value = Inf)
  for (th in th_grid) {
    for (aPE in aPE_grid) {
      for (st in ab_starts) {
        f <- tryCatch(
          optim(par = st,
                fn = function(p) imprswitchNLL(c(p, aPE, th), d$choice, d$reward),
                method = "L-BFGS-B",
                lower = c(0.001, 0.0001), upper = c(1, 10)),
          error = function(e) NULL)
        if (!is.null(f) && f$value < best$value) {
          best <- list(value = f$value,
                       par = c(f$par, alphaPE = aPE, pe_threshold = th))
        }
      }
    }
  }
  best
}

################################################################################################################################################################
# fit function: grid for alphaPE, optim for alpha and beta, pe_threshold fixed
fit_grid <- function(d,
                     th_grid = -0.2,
                     aPE_grid = seq(0.01, 0.50, by = 0.02),
                     ab_starts = list(c(alpha = 0.3, beta = 3),
                                      c(alpha = 0.6, beta = 1.5),
                                      c(alpha = 0.1, beta = 6))) {
  best <- list(value = Inf)
  for (th in th_grid) {
    for (aPE in aPE_grid) {
      for (st in ab_starts) {
        f <- tryCatch(
          optim(par = st,
                fn = function(p) imprswitchNLL(c(p, aPE, th), d$choice, d$reward),
                method = "L-BFGS-B",
                lower = c(0.001, 0.0001), upper = c(1, 10)),
          error = function(e) NULL)
        if (!is.null(f) && f$value < best$value) {
          best <- list(value = f$value,
                       par = c(f$par, alphaPE = aPE, pe_threshold = th))
        }
      }
    }
  }
  best
}

################################################################################################################################################################
# timing check: how long does it take to run for 10 sims?

library(parallel)
n_cores <- detectCores() - 1

system.time(
  test_fits <- mclapply(1:10, function(i) fit_grid(imprswitch_list[[i]]), 
                        mc.cores = n_cores)
)

################################################################################################################################################################
# parameter recovery
fits <- mclapply(1:n_sims, function(i) fit_grid(imprswitch_list[[i]]),
                 mc.cores = n_cores)
saveRDS(fits, "fits_impr_switch.rds")

get_par <- function(b, name) if (is.finite(b$value)) unname(b$par[name]) else NA_real_

sum_impr_switch$recovered_alpha           <- sapply(fits, get_par, name = "alpha")
sum_impr_switch$recovered_beta            <- sapply(fits, get_par, name = "beta")
sum_impr_switch$recovered_alphaPE         <- sapply(fits, get_par, name = "alphaPE")
sum_impr_switch$recovered_pe_threshold    <- sapply(fits, get_par, name = "pe_threshold")
sum_impr_switch$nll_fit                   <- sapply(fits, function(b) b$value)

head(sum_impr_switch)

################################################################################################################################################################
# nlls vergelijken
sum_impr_switch$nll_true <- sapply(1:n_sims, function(i) {
  d <- imprswitch_list[[i]]
  imprswitchNLL(c(true_alpha[i], true_beta[i], true_alphaPE[i], true_pe_threshold[i]),
                d$choice, d$reward)
})
mean(sum_impr_switch$nll_fit > sum_impr_switch$nll_true + 0.5)

sum(sum_impr_switch$recovered_alpha < 0.01 & sum_impr_switch$recovered_beta > 9.9, na.rm = TRUE)

################################################################################################################################################################
# recovery plots
par(mfrow = c(1, 3))
rec_plot(sum_impr_switch$alpha,        sum_impr_switch$recovered_alpha,          "alpha")
rec_plot(sum_impr_switch$beta,         sum_impr_switch$recovered_beta,           "beta")
rec_plot(sum_impr_switch$alphaPE,      sum_impr_switch$recovered_alphaPE,        "alphaPE")
rec_plot(sum_impr_switch$pe_threshold, sum_impr_switch$recovered_pe_threshold,   "pe_threshold")

