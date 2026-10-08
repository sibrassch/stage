# start week 8
# main goals: change the value update
#             do the model recovery (RW vs SWITCH2)
#             get familiar with the hpc

# trying out new function without value update right before switch
set.seed(2525)
par(mfrow = c(2, 1))
sim1 <- simulate_switch2(
  alpha               = 0.1,
  beta                = 10,
  alphaPE             = 0.2,
  pe_threshold        = -0.2
)
valueplot(sim1)

# model recovery
library(parallel)
n_cores <- detectCores() - 1

task <- list(n_trials = 400, 
             correct_stim = "green",
             reversal_trial = seq(25, 400, by = 25),
             p_reward_correct = 0.90,
             p_reward_incorrect = 0.10)

# simulate data
n_per_model <- 100
set.seed(123)

par_rw <- data.frame(alpha   = runif(n_per_model, 0.05, 0.95),
                     beta    = runif(n_per_model, 0.5, 10))
par_sw <- data.frame(alpha   = runif(n_per_model, 0.05, 0.95),
                     beta    = runif(n_per_model, 0.5, 10),
                     th      = runif(n_per_model, -0.35, -0.18))

data_rw <- lapply(1:n_per_model, function(i)
  do.call(simulate_RW, c(task, list(alpha = par_rw$alpha[i], beta = par_rw$beta[i]))))
data_sw <- lapply(1:n_per_model, function(i)
  do.call(simulate_switch2, c(task, list(alpha = par_sw$alpha[i], beta = par_sw$beta[i],
                                    alphaPE = 0.2, pe_threshold = par_sw$th[i]))))
summary(sapply(data_sw, function(d) sum(d$switch_dec)))

# fit functions
ab_starts <- list(c(alpha = 0.3, beta = 3), 
                  c(alpha = 0.7, beta = 1.5), 
                  c(alpha = 0.1, beta = 6))
ab_lower <- c(0.001, 0.001)
ab_upper <- c(1, 20)
th_grid <- seq(-0.45, -0.1, by = 0.02)

fit_rw <- function(d) {
  best <- list(value = Inf)
  for (st in ab_starts) {
    f <- tryCatch(optim(st, neglogL, choice = d$choice, reward = d$reward,
                        method = "L-BFGS-B", lower = ab_lower, upper = ab_upper),
                  error = function(e) NULL)
    if (!is.null(f) && f$value < best$value) best <- list(value = f$value, par = f$par)
  }
  best
}

fit_sw <- function(d) {
  best <- list(value = Inf)
  for (th in th_grid) {
    for (st in ab_starts) {
      f <- tryCatch(optim(st, function(p) switch2NLL(c(p, 0.2, th), d$choice, d$reward),
                          method = "L-BFGS-B", lower = ab_lower, upper = ab_upper),
                    error = function(e) NULL)
      if (!is.null(f) && f$value < best$value) best <- list(value = f$value, par = c(f$par, pe_threshold = th))
    }
  }
  best
}
  
# fit all data with both models
all_data <- c(data_rw, data_sw)
generated <- rep(c("RW", "Switch"), each = n_per_model)
fit_both <- function(d) list(rw = fit_rw(d), sw = fit_sw(d))

system.time(test <- mclapply(all_data[c(1:3, 101:103)], fit_both, mc.cores = n_cores))

run_time <- system.time(
  fits_mc <- mclapply(all_data, fit_both, mc.cores = n_cores)
)
saveRDS(list(fits = fits_mc, run_time = run_time), "fits_model_comparison.rds")
run_time["elapsed"]/60

# analysis and confusion matrix (= a table that measures how well a machine learning 
#                                  classification model performs by comparing its 
#                                  predicted labels against the actual, true values)

# BICs (RW has 2, switch has 3 parameters)
# BIC = 2 * NLL + k * log(n) 
mc$bic_rw <- 2 * mc$nll_rw + 2 * log(n_trials)
mc$bic_sw <- 2 * mc$nll_sw + 3 * log(n_trials)
mc$best <- ifelse(mc$bic_rw < mc$bic_sw, "RW", "Switch")
mc$delta <- mc$bic_rw - mc$bic_sw                            # if > 0, switch better than

# confusion matrix
conf <- table(generated = mc$generated, best_fit = mc$best)
conf
