# GOAL1: met RW model data genereren
# GOAL2: eventueel al proberen alpha en beta schatten

# heel simpele butterfly task
# model: V_stim(t+1) = V_stim(t) + alpha * (reward(t) - V_stim(t))
# softmax: P(green) = exp(beta * V_green) / (exp(beta * V_green) + exp(beta * V_blue))
# parameters: alpha = learning rate
#             beta = inverse temperature 


# HELPER FUNCTIONS
softmax <- function(V, beta) {
  exp(beta * V["green"]) /
    (exp(beta * V["green"]) + exp(beta * V["blue"]))
}
deliver_reward <- function(is_correct, p_reward_correct, p_reward_incorrect) {
  p_rew <- if (is_correct) p_reward_correct else p_reward_incorrect
  reward <- as.numeric(runif(1) < p_rew)
}
value_update <- function(V, choice, reward, alpha) {
  pred_error <- reward - V[choice]
  V[choice] <- V[choice] + alpha * pred_error
  V
}

# TASK
simulate_RW_task <- function(n_trials        = 100,
                             correct_stim    = "green",
                             p_reward_correct   = 0.8,
                             p_reward_incorrect = 0.2,
                             alpha           = 0.2,
                             beta            = 3,
                             V_init          = c(green = 0, blue = 0)) {
  V <- V_init
  
  results <- data.frame(
    trial          = 1:n_trials,
    choice         = character(n_trials),
    correct        = logical(n_trials),
    reward         = numeric(n_trials),
    V_green        = numeric(n_trials),
    V_blue         = numeric(n_trials),
    p_choose_green = numeric(n_trials)
  )
  
  for (t in 1:n_trials) {
    
    # softmax choice prob voor green
    p_green <- softmax(V, beta)
    
    # agent makes choice
    choice <- ifelse(runif(1) < p_green, "green", "blue")
    is_correct <- (choice == correct_stim)
    
    # after choice we get reward delivery
    reward <- deliver_reward(is_correct, p_reward_correct, p_reward_incorrect)
    
    # na reward komt value update
    V <- value_update(V, choice, reward, alpha)
    
    # log elke trial in de data frame
    results$choice[t] <- choice
    results$correct[t] <- is_correct
    results$reward[t] <- reward
    results$V_green[t] <- V["green"]
    results$V_blue[t] <- V["blue"]
    results$p_choose_green[t] <- p_green
  }
  
  # voeg attribuut "parameters" toe aan dataframe zodat exacte waarden van huidig gerunde simulatie opgeslaan worden
  attr(results, "parameters") <- list(correct_stim = correct_stim, p_reward_correct = p_reward_correct, 
                                      p_reward_incorrect = p_reward_incorrect, alpha = alpha, beta = beta)
  results
}

# sim runnen
sim <- simulate_RW_task(
  n_trials           = 150,
  correct_stim       = "green",
  p_reward_correct   = 0.8,
  p_reward_incorrect = 0.2,
  alpha              = 0.2,
  beta               = 3
)

head(sim)

# plots maken
params <- attr(sim, "parameters")
plot(sim$trial, sim$V_green, type = "l", col = "forestgreen", lwd = "2",
     ylim = c(0, 1), xlab = "Trial", ylab = "Associative strength (V)",
     main = "Rescorla-Wagner learning curves", bty = "l")
mtext(sprintf("alpha = %.2f, beta = %.1f", params$alpha, params$beta), side = 3, line = 0.3, cex = 0.85)
lines(sim$trial, sim$V_blue, col = "dodgerblue", lwd = "2")
legend("bottomright", legend = c("Green butterfly", "Blue butterfly"), 
       col = c("forestgreen", "dodgerblue"), lwd = "2", bty = "n")


# data genereren: sim 1000 keer runnen
set.seed(202)
n_sims <- 1000

true_alpha <- runif(n_sims, min = 0, max = 1)
true_beta  <- runif(n_sims, min = 0, max = 10)

sim_list <- vector("list", n_sims) # creating a list of 1000 dataframes (one for every output of 100 trials)

for (i in 1:n_sims) {
  sim_list[[i]] <- simulate_RW_task(
    n_trials = 100,
    correct_stim = "green",
    p_reward_correct = 0.8,
    p_reward_incorrect = 0.2,
    alpha = true_alpha[i],
    beta = true_beta[i]
  )
}

# data visualiseren
summary_df <- data.frame(
  sim = 1:n_sims,
  alpha = true_alpha,
  beta = true_beta,
  prop_correct = sapply(sim_list, function(d) mean(d$correct)),
  final_Vgreen = sapply(sim_list, function(d) tail(d$V_green, 1))
)
head(summary_df)

# plot maken
beta_groups <- cut(summary_df$beta, breaks = 5,
                   labels = c("low beta", "medium low beta", "medium beta", "medium high beta", "high beta"))
group_colors <- c("low beta"         = "skyblue",
                  "medium low beta"  = "chartreuse3",
                  "medium beta"      = "gold1",
                  "medium high beta" = "darkorange",
                  "high beta"        = "firebrick")
plot(summary_df$alpha, summary_df$prop_correct,
     col = group_colors[beta_groups], pch = 16,
     xlab = "alpha", ylab = "proportion correct",
     main = "Proportion correct vs Alpha, colored by Beta",
     bty = "l")

legend("bottomright", legend = names(group_colors), col = group_colors, pch = 16, bty = "n", cex = 0.8)

# plot van enkel laatste 20 trials per sim
summary_df$prop_correct_late <- sapply(sim_list, function(d) mean(tail(d$correct, 20)))

beta_groups <- cut(summary_df$beta, breaks = 5,
                   labels = c("low beta", "medium low beta", "medium beta", "medium high beta", "high beta"))
group_colors <- c("low beta"         = "skyblue",
                  "medium low beta"  = "chartreuse3",
                  "medium beta"      = "gold1",
                  "medium high beta" = "darkorange",
                  "high beta"        = "firebrick")
plot(summary_df$alpha, jitter(summary_df$prop_correct_late, amount = 0.015),
     col = group_colors[beta_groups], pch = 16,
     xlab = "alpha", ylab = "proportion correct (last 20 trials, jittered)",
     main = "Proportion correct Late Trials vs Alpha, colored by Beta",
     bty = "l")

legend("bottomright", legend = names(group_colors), col = group_colors, pch = 16, bty = "n", cex = 0.8)
# GOAL1 X


# beginnen met estimation











