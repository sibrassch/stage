# correlatie performance met individuele parameters van model

source("/Users/sibrass/Documents/STAGE/stage/functions.R")
# generate data
n_sims <- 1000

true_alpha <- runif(n_sims, min = 0, max = 1)
true_beta  <- runif(n_sims, min = 0, max = 10)

sim_list <- vector("list", n_sims) # creating a list of 1000 dataframes (one for every output of 100 trials)

for (i in 1:n_sims) {
  sim_list[[i]] <- simulate_RW_task(
    n_trials = 200,
    correct_stim = "green",
    p_reward_correct = 0.8,
    p_reward_incorrect = 0.2,
    alpha = true_alpha[i],
    beta = true_beta[i]
  )
}

summary_df <- data.frame(
  sim = 1:n_sims,
  alpha = true_alpha,
  beta = true_beta,
  prop_correct = sapply(sim_list, function(d) mean(d$correct)),
  final_Vgreen = sapply(sim_list, function(d) tail(d$V_green, 1))
)
head(summary_df)

# correlatie parameters met performance (= accuracy)
par(mfrow = c(2, 2))
plot(summary_df$alpha, summary_df$prop_correct,
     xlab = "true alpha", ylab = "proportion correct",
     main = "Correlation of Learning Rate with Performance", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
plot(summary_df$beta, summary_df$prop_correct,
     xlab = "true beta", ylab = "proportion correct",
     main = "Correlation of Inverse Temperature with Performance", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")

# parameters recoveren
recovered_alpha   <- numeric(n_sims)
recovered_beta    <- numeric(n_sims)

for (i in 1:n_sims) {
  d <- sim_list[[i]]
  
  fit_i <- tryCatch(
    optim(
      par      = c(alpha = 0.5, beta = 1),
      fn       = neglogL,
      choice   = d$choice,
      reward   = d$reward,
      method   = "L-BFGS-B",
      lower    = c(0.001, 0.001),
      upper    = c(1, 50) 
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

summary_df$recovered_alpha <- recovered_alpha
summary_df$recovered_beta <- recovered_beta

head(summary_df)

# correlatie plots voor de recovered parameters
plot(summary_df$recovered_alpha, summary_df$prop_correct,
     xlab = "recovered alpha", ylab = "proportion correct",
     main = "Correlation of Recovered Learning Rate with Performance", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
plot(summary_df$recovered_beta, summary_df$prop_correct,
     xlab = "recovered beta", ylab = "proportion correct",
     main = "Correlation of Recovered Inverse Temperature with Performance", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
