# correlatie performantie en parameters empirische data

source("/Users/sibrass/Documents/STAGE/stage/functions.R")

# data inlezen
data_dir <- "/Users/sibrass/Documents/STAGE/start_info/reversallearning/Data/raw/"
csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
length(csv_files)
csv_files[1:5]

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

# mean accuracy per subject
meanAcc <- numeric(length(subjects))
names(meanAcc) <- subjects

for (i in subjects) {
  meanAcc[i] <- mean(complete_data$Accuracy[complete_data$subject == i], na.rm = TRUE)
}


# correlatie (plots)
par(mfrow = c(1, 2))
plot(fit_results$alpha, meanAcc,
     xlab = "alpha", ylab = "Accuracy",
     main = "Correlation of Learning Rate with Accuracy", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")
plot(fit_results$beta, meanAcc,
     xlab = "beta", ylab = "Accuracy",
     xlim = c(0, 5),
     main = "Correlation of Inverse Temperature with Performance", pch = 16, col = rgb(0, 0, 0, 0.3), bty = "l")

cor(fit_results$alpha, meanAcc, method = "pearson")
cor(fit_results$beta, meanAcc, method = "pearson")
cor(fit_results$alpha, meanAcc, method = "spearman")
cor(fit_results$beta, meanAcc, method = "spearman")
