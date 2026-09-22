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
n_sims <- 10000

set.seed(123)
true_alpha       <- runif(n_sims, min = 0, max = 1)
true_beta        <- runif(n_sims, min = 0, max = 10)
true_alphaPE     <- runif(n_sims, min = 0, max = 1)
true_pe_threshold   <- runif(n_sims, min = -1, max = -0.1)

imprswitch_list <- vector("list", n_sims) # creating a list of 1000 dataframes (one for every output of 100 trials)

# actual loop
for (i in 1:n_sims) {
  imprswitch_list[[i]] <- simulate_impr_switch(
    n_trials = 200,
    correct_stim = "green",
    reversal_trial = c(25, 50, 75, 100, 125, 150, 175, 200),
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
recovered_alpha        <- numeric(n_sims)
recovered_beta         <- numeric(n_sims)
recovered_alphaPE      <- numeric(n_sims)
recovered_pe_threshold <- numeric(n_sims)



