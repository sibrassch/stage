source("/Users/sibrass/Documents/STAGE/stage/functions.R")
# GOAL5: fit new model to data
# run 1 simulation
onesim <- simulate_switch_task(
  n_trials = 200,
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
# data inlezen
data_dir <- "/Users/sibrass/Documents/STAGE/start_info/reversallearning/Data/raw/"
csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
length(csv_files)
csv_files[1:5]

# data cleaning
all_data <- lapply(csv_files, read_one_subject)
complete_data <- do.call(rbind, all_data)

# maak flower-butterfly-correct response mapping duidelijk
rule3_map <- unique(complete_data[complete_data$Rule == 3, c("Stimulus", "CorrResp")])
names(rule3_map)[2] <- "rule3_key" # rename the CorrResp column in rule3_map

complete_data <- merge(complete_data, rule3_map, by = "Stimulus")

complete_data$key_pressed <- ifelse(complete_data$Response == 70, "f",
                                    ifelse(complete_data$Response == 74, "j", NA))

complete_data$choice <- ifelse(complete_data$key_pressed == complete_data$rule3_key, "green", "blue")

# reorganise complete_data by subject again instead of stimulus type (door merge is data nu organised op stim)
complete_data <- complete_data[order(complete_data$subject, complete_data$trial_index), ]

# MODEL FIT: alpha en beta
subjects <- unique(complete_data$subject)
fit_results <- data.frame(subject = subjects, alpha = NA, beta = NA)

for (s in subjects) {
  sd <- complete_data[complete_data$subject == s, ]
  f <- tryCatch(
    optim(
      par = c(alpha = 0.5, beta = 1),
      fn = switchNLL,
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

hist(fit_results$alpha)
hist(fit_results$beta)

# MODEL COMPARISON: value switch vs old
model_comparison <- data.frame(
  subject      = subjects,
  n_trials     = NA,
  alpha_switch = NA, beta_switch = NA, logL_switch = NA,   # start everything as NA so that when an 
  alpha_old    = NA, beta_old    = NA, logL_old    = NA    # error occurs in one subject, it doesn't leave a gap but "NA"
)

for (s in subjects) {
  sd <- complete_data[complete_data$subject == s, ]       # keeps only the rows for that specific s, ALL columns (empty space after ,)
  sd <- sd[!is.na(sd$choice) & !is.na(sd$reward), ]          # keeps only rows where choice & reward are NOT NA
  model_comparison$n_trials[model_comparison$subject == s] <- nrow(sd) # n_trials needed for BIC
  
  fit_switch <- tryCatch(
    optim(par = c(alpha = 0.5, beta = 1),
          fn = switchNLL,
          choice = sd$choice,
          reward = sd$reward,
          method = "L-BFGS-B",
          lower = c(0.001, 0.001), upper = c(1, 20)),
          error = function(e) NULL
  )
  fit_old <- tryCatch(
    optim(par = c(alpha = 0.5, beta = 1),
          fn = neglogL,
          choice = sd$choice,
          reward = sd$reward,
          method = "L-BFGS-B",
          lower = c(0.001, 0.001), upper = c(1, 20)),
    error = function(e) NULL
  )
  if (!is.null(fit_switch)) {
    model_comparison$alpha_switch[model_comparison$subject == s] <- fit_switch$par["alpha"]
    model_comparison$beta_switch[model_comparison$subject == s] <- fit_switch$par["beta"]
    model_comparison$logL_switch[model_comparison$subject == s] <- -fit_switch$value
  }
  if (!is.null(fit_old)) {
    model_comparison$alpha_old[model_comparison$subject == s] <- fit_old$par["alpha"]
    model_comparison$beta_old[model_comparison$subject == s] <- fit_old$par["beta"]
    model_comparison$logL_old[model_comparison$subject == s] <- -fit_old$value
  }
}

# computing AIC and BIC
# both models have 2 free parameters (alpha & beta)
# AIC = -2 * log(likelihood) + 2k
# BIC = -2 * log(likelihood) + k*log(n)
k <- 2

model_comparison$AIC_switch <- (-2) * model_comparison$logL_switch + 2 * k
model_comparison$AIC_old    <- (-2) * model_comparison$logL_old + 2 * k
model_comparison$BIC_switch <- (-2) * model_comparison$logL_switch + k*log(model_comparison$n_trials)
model_comparison$BIC_old    <- (-2) * model_comparison$logL_old + k*log(model_comparison$n_trials)

# compare AIC & BIC
model_comparison$AIC_comp <- model_comparison$AIC_switch - model_comparison$AIC_old
model_comparison$BIC_comp <- model_comparison$BIC_switch - model_comparison$BIC_old

sum(model_comparison$AIC_comp < 0, na.rm = TRUE) # all subjects better fit by switch model
sum(model_comparison$AIC_comp > 0, na.rm = TRUE) # all subjects better fit by old model

t.test(model_comparison$logL_switch, model_comparison$logL_old, paired = TRUE)

# plot the comparison
par(mfrow = c(1, 1))
plot(model_comparison$logL_old, model_comparison$logL_switch,
     xlab = "log-likelihood: old model", ylab = "log-likelihood: switch model",
     main = "Model Comparison: Old vs. Switch",
     pch = 16, col = rgb(0, 0, 0, 0.4), bty = "l")
abline(0, 1, col = "red", lty = 2)