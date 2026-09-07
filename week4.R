# GOAL 1: parameter estimation                      X (vanaf lijn 175)
# GOAL 2: plot correlation estimated vs real values X (vanaf lijn 214)
# GOAL 3: model fitten op echte data                X (vanaf lijn 236)
# GOAL 4: second set of p_reward in model           X (vanaf lijn 347)
# GOAL 5: make model switch based on feedback

# copy paste RW model voor MLE
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
simulate_RW_task <- function(n_trials        = 1000,
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
n_sims <- 1000

true_alpha <- runif(n_sims, min = 0, max = 1)
true_beta  <- runif(n_sims, min = 0, max = 10)

sim_list <- vector("list", n_sims) # creating a list of 1000 dataframes (one for every output of 100 trials)

for (i in 1:n_sims) {
  sim_list[[i]] <- simulate_RW_task(
    n_trials = 1000,
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

# likelihood:     L(alpha, beta)    = ∏_{t=1}^{n} P_green(t)^I(c_t = green) * P_blue(t)^I(c_t = blue)
# loglikelihood:  logL(alpha, beta) = Σ_{t=1}^{n} [I(c_t = green) * log(P_green(t)) + I(c_t = blue) * log(blue(t))]
neglogL <- function(params, choice, reward, V_init = c(green = 0, blue = 0)) {
  alpha   <- params[1]
  beta    <- params[2]
  
  V       <- V_init
  n       <- length(choice)
  loglik  <- 0 # starting value, loop will update
  
  for (t in 1:n) {
    p_green <- softmax(V, beta)
    p_chosen <- if (choice[t] == "green") p_green else (1 - p_green)
    loglik <- loglik + log(p_chosen)
    
    V <- value_update(V, choice[t], reward[t], alpha)
  }
  
  -loglik
}

fit <- optim(
  par = c(alpha = 0.2, beta = 1),
  fn = neglogL,
  choice = sim$choice,
  reward = sim$reward,
  method = "L-BFGS-B",
  lower = c(0.001, 0.001),
  upper = c(1, 20)
)

fit$par
params$alpha
params$beta

# parameter recovery
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
# GOAL 1 X




# correlatie plot
par(mfrow = c(1, 2))

plot(summary_df$alpha, summary_df$recovered_alpha,
     xlab = "true alpha", ylab = "recovered alpha",
     main = "Alpha Recovery", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
abline(0, 1, col = "red", lty = 2)

plot(summary_df$beta, summary_df$recovered_beta,
     xlab = "true beta", ylab = "recovered beta",
     main = "Beta Recovery", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
abline(0, 1, col = "red", lty = 2)
# GOAL 2 X




# model fitten op echte data
# verschillen:
# reward --> Points     --> passen we aan op lijn 252
# choice --> Response 

# data inlezen
data_dir <- "/Users/sibrass/Documents/STAGE/reversallearning/Data/raw/"
csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
length(csv_files)
csv_files[1:5]

read_one_subject <- function(file_path) {
  data          <- read.csv(file_path)
  
  # remove practice data
  data          <- data[startsWith(data$Stimulus, "Test"), ]
  
  # remove obersvations with NA or null
  data          <- data[!is.na(data$Accuracy),]
  data          <- data[!data$ResponseTime == "null",]
  
  # rt must be higher than 0.4
  data          <- data[data$rt > .4, ]
  
  # set reward to 0 and 1
  data$reward   <- data$Points/10
  
  # make stimulus binary
  data$stimulus <- ifelse(data$Stimulus %in% c("Test_1", "Test_3"), 1, 2)
  
  # tag every row with subject identifier
  data$subject  <- basename(file_path)
  
  # return data (the cleaned dataframe)
  data
}

all_data <- lapply(csv_files, read_one_subject)
complete_data <- do.call(rbind, all_data)

nrow(complete_data)
head(complete_data)

# maak flower-butterfly-correct response mapping duidelijk
rule3_map <- unique(complete_data[complete_data$Rule == 3, c("Stimulus", "CorrResp")])
names(rule3_map)[2] <- "rule3_key" # rename the CorrResp column in rule3_map

complete_data <- merge(complete_data, rule3_map, by = "Stimulus")

complete_data$key_pressed <- ifelse(complete_data$Response == 70, "f",
                                    ifelse(complete_data$Response == 74, "j", NA))

complete_data$choice <- ifelse(complete_data$key_pressed == complete_data$rule3_key, "green", "blue")

# reorganise complete_data by subject again instead of stimulus type (door merge is data nu organised op stim)
complete_data <- complete_data[order(complete_data$subject, complete_data$trial_index), ]

# quick sanity check before fit
table(complete_data$choice)
table(complete_data$reward)

# MODEL FIT
subjects <- unique(complete_data$subject)
fit_results <- data.frame(subject = subjects, alpha = NA, beta = NA)

for (s in subjects) {
  sd <- complete_data[complete_data$subject == s, ]
  f <- tryCatch(
    optim(
      par = c(alpha = 0.5, beta = 1),
      fn = neglogL,
      choice = sd$choice,
      reward = sd$reward,
      method = "L-BFGS-B",
      lower = c(0.001, 0.001),
      upper = c(1, 20)
    ),
    error = function(e) NULL
  )
  
  if (!is.null(f)) {
    fit_results$alpha[fit_results$subject == s] <- f$par["alpha"]
    fit_results$beta[fit_results$subject == s] <- f$par["beta"]
  }
}
head(fit_results)

summary(fit_results$alpha)
summary(fit_results$beta)
hist(fit_results$alpha)
hist(fit_results$beta)

# now actual model fit:
# McFaddens R squared: 1 - (logLfull / logLnull)
fit_results$pseudo_R2 <- NA

for (i in seq_len(nrow(fit_results))) {
  s <- fit_results$subject[i]
  sd <- complete_data[complete_data$subject == s, ]
  
  chose_green <- as.numeric(sd$choice == "green")
  null_p <- mean(chose_green)
  null_negloglik <- -sum(chose_green * log(null_p) + (1 - chose_green) * log(1 - null_p))
  
  fitted_negloglik <- neglogL(c(fit_results$alpha[i], fit_results$beta[i]), sd$choice, sd$reward)
  
  fit_results$pseudo_R2[i] <- 1 - (fitted_negloglik / null_negloglik)
  
}

summary(fit_results$pseudo_R2)
hist(fit_results$pseudo_R2, main = "Model fit across subjects", xlab = "pseudo-R²")
# GOAL 3 X



# extra set Vs introduceren in eigen model
# model blijft zo goed als gelijk, er wordt gewoon geswitched tussen "correct" en "incorrect" antwoord
# TASK
simulate_RW_task <- function(n_trials           = 220,
                             correct_stim       = "green",
                             reversal_trial     = c(21, 41, 61, 81, 101, 121, 141), 
                             p_reward_correct   = 0.8,
                             p_reward_incorrect = 0.2,
                             alpha              = 0.2,
                             beta               = 3,
                             V_init             = c(green = 0, blue = 0)) {
  V <- V_init
  current_correct <- correct_stim
  
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
    
    # flip when we hit reversal trial
    if (!is.null(reversal_trial) && t %in% reversal_trial) {
      current_correct <- ifelse(current_correct == "green", "blue", "green")
    }
    
    p_green <- softmax(V, beta)
    choice <- ifelse(runif(1) < p_green, "green", "blue")
    is_correct <- (choice == current_correct)
    
    reward <- deliver_reward(is_correct, p_reward_correct, p_reward_incorrect)
    V <- value_update(V, choice, reward, alpha)
    
    results$choice[t]         <- choice
    results$correct[t]        <- is_correct
    results$reward[t]         <- reward
    results$V_green[t]        <- V["green"]
    results$V_blue[t]         <- V["blue"]
    results$p_choose_green[t] <- p_green
  }
  
  # voeg attribuut "parameters" toe aan dataframe zodat exacte waarden van huidig gerunde simulatie opgeslaan worden
  attr(results, "parameters") <- list(correct_stim = correct_stim, reversal_trial = reversal_trial,
                                      p_reward_correct = p_reward_correct, 
                                      p_reward_incorrect = p_reward_incorrect, 
                                      alpha = alpha, beta = beta)
  results
}

# nieuwe sim runnen
sim_reversal <- simulate_RW_task(
  n_trials           = 150,
  correct_stim       = "green",
  reversal_trial     = c(21, 41, 61, 81, 101, 121, 141),
  p_reward_correct   = 0.8,
  p_reward_incorrect = 0.2,
  alpha              = 0.2,
  beta               = 3
)

# ook plotten
params <- attr(sim_reversal, "parameters")
plot(sim_reversal$trial, sim_reversal$V_green, type = "l", col = "forestgreen", lwd = "2",
     ylim = c(0, 1), xlab = "Trial", ylab = "Associative strength (V)",
     main = "Rescorla-Wagner reversal learning curves", bty = "l")
mtext(sprintf("alpha = %.2f, beta = %.1f", params$alpha, params$beta), side = 3, line = 0.3, cex = 0.85)
lines(sim_reversal$trial, sim_reversal$V_blue, col = "dodgerblue", lwd = "2")
legend("bottomright", legend = c("Green butterfly", "Blue butterfly"), 
       col = c("forestgreen", "dodgerblue"), lwd = "2", bty = "n")
# GOAL 4 X



# model laten switchen van reward probabilities obv feedback en niet vast aantal trials
