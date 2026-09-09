# reusable functions

# softmax rule========================================================================================================================================
softmax <- function(V, beta) {
  exp(beta * V["green"]) /
    (exp(beta * V["green"]) + exp(beta * V["blue"]))
}

# reward delivery for RW model========================================================================================================================
deliver_reward <- function(is_correct, p_reward_correct, p_reward_incorrect) {
  p_rew <- if (is_correct) p_reward_correct else p_reward_incorrect
  reward <- as.numeric(runif(1) < p_rew)
}

# value update========================================================================================================================================
value_update <- function(V, choice, reward, alpha) {
  pred_error <- reward - V[choice]
  V[choice] <- V[choice] + alpha * pred_error
  V
}

# RW model============================================================================================================================================
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

# negative logLikelihood==============================================================================================================================
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

# read data ==========================================================================================================================================
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
