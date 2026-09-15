# GOAL1: make agent switch too                              X
# GOAL2: correlatie accuracy en parameters empirische data  X
          # (script week5_3)
# GOAL3: generate data with new model                       X
# GOAL4: parameter recovery of new model                    X
# GOAL5: fit new model to data
# GOAL6: make new model more complex with prior belief

# UITBREIDING:
# niet alleen de task switchet de values om de zo veel trials
# de agent heeft ook 2 sets values nodig om tussen te switchen
# zo worden de values voor green en blue niet steeds aangepast obv de huidige rule
# maar worden 2 sets values apart aangepast elk binnen hun eigen rule

source("/Users/sibrass/Documents/STAGE/stage/functions.R")

# 2 value sets:
# V_A: values when model thinks green is currently rewarded color
# V_B: values when model thinks blue

# simulation function
simulate_switch_task <- function(n_trials            = 220,
                                 correct_stim        = "green",
                                 reversal_trial      = c(25, 50, 75, 100, 125, 150, 175, 200),
                                 p_reward_correct    = 0.80,
                                 p_reward_incorrect  = 0.20,
                                 alpha               = 0.2,
                                 beta                = 3,
                                 threshold           = 3,
                                 V_A_init            = c(green = 0.5, blue = 0.5),
                                 V_B_init            = c(green = 0.5, blue = 0.5),
                                 active_state_init          = "A") {
  V_A                 <- V_A_init
  V_B                 <- V_B_init
  active_state        <- active_state_init
  unrewarded_streak   <- 0
  trials_since_switch <- 0
  current_correct <- correct_stim
  
  results <- data.frame(
    trial            = 1:n_trials,
    true_state       = character(n_trials),  # the actual state A or B of the task
    active_state     = character(n_trials),  # the state the model assumes it is in
    choice           = character(n_trials),
    correct          = logical(n_trials),
    reward           = numeric(n_trials),
    V_A_green        = numeric(n_trials),
    V_A_blue         = numeric(n_trials),
    V_B_green        = numeric(n_trials),
    V_B_blue         = numeric(n_trials),
    p_choose_green   = numeric(n_trials),
    switch_dec       = logical(n_trials)     # switch decision: did model decide to trigger switch?
  )
  
  for (t in 1:n_trials) {
    
    # flip when we hit reversal trial
    if (!is.null(reversal_trial) && t %in% reversal_trial) {
      current_correct <- ifelse(current_correct == "green", "blue", "green")
    }
    
    # capture and use correct active (model) state
    active_state_used <- active_state
    V_active <- if (active_state == "A") V_A else V_B
    
    p_green <- softmax(V_active, beta)
    choice <- ifelse(runif(1) < p_green, "green", "blue")
    is_correct <- (choice == current_correct)
    
    reward <- deliver_reward(is_correct, p_reward_correct, p_reward_incorrect)
    
    # update values of the active state
    if (active_state == "A") {
      V_A <- value_update(V_A, choice, reward, alpha)
    } else {
      V_B <- value_update(V_B, choice, reward, alpha)
    }
    
    trials_since_switch <- trials_since_switch + 1
    
    # count unrewarded trials to trigger a switch of active state
    if (reward == 0) {
      unrewarded_streak <- unrewarded_streak + 1
    } else {
      unrewarded_streak <- 0
    }
    
    switch_dec <- FALSE
    
    # trigger switch after unrewarded streak reaches threshold
    if (unrewarded_streak >= threshold) {
      active_state <- ifelse(active_state == "A", "B", "A")
      unrewarded_streak <- 0
      trials_since_switch <- 0
      switch_dec <- TRUE
    }
    
    # log everything in results
    results$true_state[t]       <- current_correct
    results$active_state[t]     <- active_state_used
    results$choice[t]           <- choice
    results$correct[t]          <- is_correct
    results$reward[t]           <- reward
    results$V_A_green[t]        <- V_A["green"]
    results$V_A_blue[t]         <- V_A["blue"]
    results$V_B_green[t]        <- V_B["green"]
    results$V_B_blue[t]         <- V_B["blue"]
    results$p_choose_green[t]   <- p_green
    results$switch_dec[t]       <- switch_dec
  }
  
  attr(results, "parameters") <- list(correct_stim = correct_stim, reversal_trial = reversal_trial,
                                      p_reward_correct = p_reward_correct, p_reward_incorrect = p_reward_incorrect, 
                                      alpha = alpha, beta = beta, threshold = threshold, 
                                      V_A_init = V_A_init, V_B_init = V_B_init, active_state_init = active_state_init)
  results
}

# run sim of the new model
sim_switch <- simulate_switch_task(
  n_trials = 150,
  correct_stim = 'green',
  reversal_trial      = c(25, 50, 75, 100, 125, 150, 175, 200),
  p_reward_correct    = 0.80,
  p_reward_incorrect  = 0.20,
  alpha               = 0.2,
  beta                = 3,
  V_A_init = c(green = 0.5, blue = 0.5),
  V_B_init = c(green = 0.5, blue = 0.5),
  threshold           = 3
)

#plot
params <- attr(sim_switch, "parameters")
par(mfrow = c(2, 1))
plot(sim_switch$trial, sim_switch$V_A_blue, 
     xlab = "Trial", ylab = "Values in state A",
     bty = "l", type = "l", col = "dodgerblue",
     lwd = "2", ylim = c(0, 1))
lines(sim_switch$trial, sim_switch$V_A_green, col = "forestgreen",
      lwd = "2")
mtext(sprintf("0-reward threshold = %d", params$threshold), side = 3, line = 0.3, cex = 0.85)

plot(sim_switch$trial, sim_switch$V_B_blue, 
     xlab = "Trial", ylab = "Values in state B",
     bty = "l", type = "l", col = "dodgerblue",
     lwd = "2", ylim = c(0, 1), lty = 2)
lines(sim_switch$trial, sim_switch$V_B_green, col = "forestgreen",
      lwd = "2", lty = 2)
mtext(sprintf("0-reward threshold = %d", params$threshold), side = 3, line = 0.3, cex = 0.85)

# generate date with new model
n_sims <- 1000

set.seed(123)
true_alpha <- runif(n_sims, min = 0, max = 1)
true_beta  <- runif(n_sims, min = 0, max = 10)

switch_sim_list <- vector("list", n_sims) # creating a list of 1000 dataframes (one for every output of 100 trials)

for (i in 1:n_sims) {
  switch_sim_list[[i]] <- simulate_switch_task(
    n_trials = 150,
    correct_stim = 'green',
    reversal_trial      = c(25, 50, 75, 100, 125, 150, 175, 200),
    p_reward_correct    = 0.80,
    p_reward_incorrect  = 0.20,
    alpha = true_alpha[i],
    beta = true_beta[i],
    V_A_init = c(green = 0.5, blue = 0.5),
    V_B_init = c(green = 0.5, blue = 0.5),
    threshold           = 3
  )
}

summary_switch_df <- data.frame(
  sim = 1:n_sims,
  alpha = true_alpha,
  beta = true_beta,
  prop_correct = sapply(switch_sim_list, function(d) mean(d$correct)),
  final_VAgreen = sapply(switch_sim_list, function(d) tail(d$V_A_green, 1)),
  final_VBblue = sapply(switch_sim_list, function(d) tail(d$V_B_blue, 1))
)
head(summary_switch_df)

# correlatie parameters met performance (= accuracy)
par(mfrow = c(1, 2))
plot(summary_switch_df$alpha, summary_switch_df$prop_correct,
     xlab = "true alpha", ylab = "proportion correct",
     main = "Correlation of Learning Rate with Performance", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
plot(summary_switch_df$beta, summary_switch_df$prop_correct,
     xlab = "true beta", ylab = "proportion correct",
     main = "Correlation of Inverse Temperature with Performance", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")

# parameter recovery
recovered_alpha   <- numeric(n_sims)
recovered_beta    <- numeric(n_sims)

for (i in 1:n_sims) {
  d <- switch_sim_list[[i]]
  
  fit_i <- tryCatch(
    optim(
      par      = c(alpha = 0.5, beta = 1),
      fn       = neglogL,
      choice   = d$choice,
      reward   = d$reward,
      method   = "L-BFGS-B",
      lower    = c(0.001, 0.001),
      upper    = c(1, 10) 
    ),
    error = function(e) NULL
  )
  
  if (!is.null(fit_i)) {
    recovered_alpha[i]  <- fit_i$par["alpha"]
    recovered_beta[i]   <- fit_i$par["beta"]
  }
  else {
    recovered_alpha[i]  <- NA
    recovered_beta[i]   <- NA
  }
}

summary_switch_df$recovered_alpha <- recovered_alpha
summary_switch_df$recovered_beta <- recovered_beta

# plot voor correlatie true en recovered alpha en beta
par(mfrow = c(1, 2))
plot(summary_switch_df$alpha, summary_switch_df$recovered_alpha,
     xlab = "true alpha", ylab = "recovered alpha",
     main = "Correlation of True vs Recovered Alpha", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
abline(0, 1, col = "red", lty = 2)

plot(summary_switch_df$beta, summary_switch_df$recovered_beta,
     xlab = "true beta", ylab = "recovered beta",
     main = "Correlation of True vs Recovered Beta", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
abline(0, 1, col = "red", lty = 2)

# plot voor correlatie recovered params en performance
par(mfrow = c(1, 2))
plot(summary_switch_df$recovered_alpha, summary_switch_df$prop_correct,
     xlab = "recovered alpha", ylab = "proportion correct",
     main = "Correlation of Performance vs Recovered Alpha", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")

plot(summary_switch_df$recovered_beta,summary_switch_df$prop_correct,
     xlab = "recovered beta", ylab = "proportion correct",
     main = "Correlation of Performance vs Recovered Beta", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")

