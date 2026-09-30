# GOAL1: make new model more complex w prediction error sum         X
source("/Users/sibrass/Documents/STAGE/stage/functions.R")
# GOAL2: parameter recovery of 4 parameters in new model
# GOAL3: new model fit to data
# GOAL4: model comparison with other switch model

# new simulation function: simulate_impr_switch()
  # new function now uses prediction error sum instead of fixed threshold
# new valueplot function: valueplot() (old one is now simplevalueplot())
  # puts dotted lines at the real task switches
  # puts colored blocks in background to indicate active states

# tryout new functions
set.seed(1234)
impr_sim1 <- simulate_impr_switch()
par(mfrow = c(2, 1))
valueplot(impr_sim1)

# generate data with new model
n_sims <- 200

set.seed(123)
true_alpha       <- runif(n_sims, min = 0, max = 1)
true_beta        <- runif(n_sims, min = 0, max = 10)
true_alphaPE     <- runif(n_sims, min = 0, max = 1)
true_pe_threshold   <- runif(n_sims, min = -0.45, max = -0.05)

imprswitch_list <- vector("list", n_sims) # creating a list of 200 dataframes (one for every output of 100 trials)

# actual loop
for (i in 1:n_sims) {
  imprswitch_list[[i]] <- simulate_impr_switch(
    n_trials = 400,
    correct_stim = "green",
    reversal_trial = c(25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350, 375),
    p_reward_correct = 0.90,
    p_reward_incorrect = 0.10,
    alpha = true_alpha[i],
    beta = true_beta[i],
    alphaPE = true_alphaPE[i],
    pe_threshold = true_pe_threshold[i],
    V_A_init = c(green = 0.5, blue = 0.5),
    V_B_init = c(green = 0.5, blue = 0.5),
    active_state_init   = "A",
    peS_init            = 0
  )
}


# summary into dataframe
sum_impr_switch <- data.frame(
  sim              = 1:n_sims,
  alpha            = true_alpha,
  beta             = true_beta,
  alphaPE          = true_alphaPE,
  pe_threshold     = true_pe_threshold,
  prop_correct     = sapply(imprswitch_list, function(d) mean(d$correct)),
  final_VAgreen    = sapply(imprswitch_list, function(d) tail(d$V_A_green, 1)),
  final_VBblue     = sapply(imprswitch_list, function(d) tail(d$V_B_blue, 1))
)

# relatie tussen accuracy en parameters
par(mfrow = c(2, 2))
plot(sum_impr_switch$alpha, sum_impr_switch$prop_correct,
     xlab = "true alpha", ylab = "proportion correct",
     main = "Relation between Learning Rate and Performance", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
plot(sum_impr_switch$beta, sum_impr_switch$prop_correct,
     xlab = "true beta", ylab = "proportion correct",
     main = "Relation between Inverse Temperature and Performance", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
plot(sum_impr_switch$alphaPE, sum_impr_switch$prop_correct,
     xlab = "true alphaPE", ylab = "proportion correct",
     main = "Relation between higher order Learning Rate and Performance", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
plot(sum_impr_switch$pe_threshold, sum_impr_switch$prop_correct,
     xlab = "true prediction error threshold", ylab = "proportion correct",
     main = "Relation between Prediction-Error Threshold and Performance", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")


# start parameter recovery
n_starts <- 5
lower <- c(alpha = 0.001, beta = 0.001, alphaPE = 0.001, pe_threshold = -1)
upper <- c(alpha = 1,     beta = 10,    alphaPE = 1,     pe_threshold = -0.001)

recovered_alpha        <- rep(NA_real_, n_sims)
recovered_beta         <- rep(NA_real_, n_sims)
recovered_alphaPE      <- rep(NA_real_, n_sims)
recovered_pe_threshold <- rep(NA_real_, n_sims)

for (i in 1:n_sims) {
  d <- imprswitch_list[[i]]
  best_fit <- NULL
  
  for (s in 1:n_starts){
    start <- runif(4, lower, upper)
    names(start) <- names(lower)
    
    fit_s <- tryCatch(
      optim(
        par = start,
        fn = imprswitchNLL,
        choice = d$choice,
        reward = d$reward,
        method = "L-BFGS-B",
        lower = lower,
        upper = upper
      ),
      error = function(e) {message("sim ", i, ", starts ", s, ": ", conditionMessage(e)); NULL}
    )
    
    if (!is.null(fit_s) && (is.null(best_fit) || fit_s$value < best_fit$value)) {
      best_fit <- fit_s
    }
  }
  

  if (!is.null(best_fit)) {
    recovered_alpha[i]        <- best_fit$par["alpha"]
    recovered_beta[i]         <- best_fit$par["beta"]
    recovered_alphaPE[i]      <- best_fit$par["alphaPE"]
    recovered_pe_threshold[i] <- best_fit$par["pe_threshold"]
    
  }
}

sum_impr_switch$recovered_alpha         <- recovered_alpha
sum_impr_switch$recovered_beta          <- recovered_beta
sum_impr_switch$recovered_alphaPE       <- recovered_alphaPE
sum_impr_switch$recovered_pe_threshold  <- recovered_pe_threshold

head(sum_impr_switch)

# plot voor relatie true en recovered alpha en beta
par(mfrow = c(2, 2))
plot(sum_impr_switch$alpha, sum_impr_switch$recovered_alpha,
     xlab = "true alpha", ylab = "recovered alpha",
     main = "True vs Recovered Alpha", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
abline(0, 1, col = "red", lty = 2)

plot(sum_impr_switch$beta, sum_impr_switch$recovered_beta,
     xlab = "true beta", ylab = "recovered beta",
     main = "True vs Recovered Beta", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
abline(0, 1, col = "red", lty = 2)

plot(sum_impr_switch$alphaPE, sum_impr_switch$recovered_alphaPE,
     xlab = "true alphaPE", ylab = "recovered alphaPE",
     main = "True vs Recovered AlphaPE", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
abline(0, 1, col = "red", lty = 2)

plot(sum_impr_switch$pe_threshold, sum_impr_switch$recovered_pe_threshold,
     xlab = "true pe_threshold", ylab = "recovered pe_threshold",
     main = "True vs Recovered Prediction-Error Threshold", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
abline(0, 1, col = "red", lty = 2)

# how many switches do we actually see in the simulations
n_switches <- sapply(imprswitch_list, function(d) sum(d$switch_dec))
sum_impr_switch$n_switches <- n_switches
summary(n_switches)
mean(n_switches == 0)
par(mfrow = c(1, 1))
plot(sum_impr_switch$pe_threshold, sum_impr_switch$alphaPE,
     cex = 0.5 + sqrt(n_switches) / 2, pch = 16, col = rgb(0, 0, 0, 0.3),
     xlab = "true pe_threshold", ylab = "true alphaPE",
     main = "pointsize = number of switches")

# plot met enkel simulations waar n_switches > 5
ok <- sum_impr_switch$n_switches >= 22
plot(sum_impr_switch$alphaPE[ok], sum_impr_switch$recovered_alphaPE[ok],
     xlab = "true alphaPE", ylab = "recovered alphaPE",
     pch = 16, col = rgb(0, 0, 0, 0.3))
abline(0, 1, col = "red", lty = 2)

plot(sum_impr_switch$pe_threshold[ok], sum_impr_switch$recovered_pe_threshold[ok],
     xlab = "true pe_threshold", ylab = "recovered pe_threshold",
     pch = 16, col = rgb(0, 0, 0, 0.3))
abline(0, 1, col = "red", lty = 2)



################################################################################
idx <- which(sum_impr_switch$n_switches >= 5)[1:20]

check <- t(sapply(idx, function(i) {
  d  <- imprswitch_list[[i]]
  tp <- c(sum_impr_switch$alpha[i], sum_impr_switch$beta[i],
          sum_impr_switch$alphaPE[i], sum_impr_switch$pe_threshold[i])
  fp <- c(sum_impr_switch$recovered_alpha[i], sum_impr_switch$recovered_beta[i],
          sum_impr_switch$recovered_alphaPE[i], sum_impr_switch$recovered_pe_threshold[i])
  c(nll_true = imprswitchNLL(tp, d$choice, d$reward),
    nll_fit  = imprswitchNLL(fp, d$choice, d$reward))
}))
check
mean(check[, 2] > check[, 1])



################################################################################
fit_grid <- function(d,
                     th_grid  = seq(-0.9, -0.05, by = 0.05),
                     aPE_grid = seq(0.05, 1, by = 0.05)) {
  best <- list(value = Inf)
  for (th in th_grid) {
    for (aPE in aPE_grid) {
      f <- tryCatch(
        optim(par = c(alpha = 0.3, beta = 3),
              fn = function(p) imprswitchNLL(c(p, aPE, th), d$choice, d$reward),
              method = "L-BFGS-B",
              lower = c(0.001, 0.001), upper = c(1, 10),
              control = list(factr = 1e10)),
        error = function(e) NULL)
      if (!is.null(f) && f$value < best$value) {
        best <- list(value = f$value,
                     par = c(f$par, alphaPE = aPE, pe_threshold = th))
      }
    }
  }
  best
}


# n_starts <- 5
# lower <- c(alpha = 0.001, beta = 0.001, alphaPE = 0.001, pe_threshold = -1)
# upper <- c(alpha = 1,     beta = 10,    alphaPE = 1,     pe_threshold = -0.001)

recovered_alpha        <- rep(NA_real_, n_sims)
recovered_beta         <- rep(NA_real_, n_sims)
recovered_alphaPE      <- rep(NA_real_, n_sims)
recovered_pe_threshold <- rep(NA_real_, n_sims)

for (i in 1:n_sims) {
  best_fit <- fit_grid(imprswitch_list[[i]])
  
  if (is.finite(best_fit$value)) {
    recovered_alpha[i]        <- best_fit$par["alpha"]
    recovered_beta[i]         <- best_fit$par["beta"]
    recovered_alphaPE[i]      <- best_fit$par["alphaPE"]
    recovered_pe_threshold[i] <- best_fit$par["pe_threshold"]
  }
  
  if (i %% 10 == 0) message("done ", i, " /", n_sims)
}

sum_impr_switch$recovered_alpha         <- recovered_alpha
sum_impr_switch$recovered_beta          <- recovered_beta
sum_impr_switch$recovered_alphaPE       <- recovered_alphaPE
sum_impr_switch$recovered_pe_threshold  <- recovered_pe_threshold

head(sum_impr_switch)

# check for 20 sims first
idx <- which(sum_impr_switch$n_switches >= 5)[1:20]

grid_res <- t(sapply(idx, function(i) {
  d  <- imprswitch_list[[i]]
  bf <- fit_grid(d)
  tp <- c(sum_impr_switch$alpha[i], sum_impr_switch$beta[i],
          sum_impr_switch$alphaPE[i], sum_impr_switch$pe_threshold[i])
  message("done sim ", i)
  c(nll_true = unname(imprswitchNLL(tp, d$choice, d$reward)),
    nll_grid = bf$value,
    true_th  = sum_impr_switch$pe_threshold[i],
    grid_th  = unname(bf$par["pe_threshold"]),
    true_aPE = sum_impr_switch$alphaPE[i],
    grid_aPE = unname(bf$par["alphaPE"]))
}))

round(grid_res, 3)
mean(grid_res[, "nll_grid"] > grid_res[, "nll_true"] + 0.5)


