# genereer gewoon muntstuk met 0.70 kans op 1
# laat muntstuk afhangen van parameters


# STAP 1: basic coin
# variabelen
samplesize = 200
prob1 = 0.7
prob0 = 0.3

# coin
coin <- c(0, 1)
toss <- sample(coin, replace = TRUE, size = samplesize, prob = c(prob0, prob1))

# results
t <- table(toss)
prop.table(t)*100
sum(toss)
data.frame(Value = names(t), Freq = as.vector(t), Proportion = as.vector(prop.table(t)))



# STAP 2: function
generate_coin <- function(prob1 = 0.7, ntrials = 100, file_name = "simulation_data.csv") {
  coin <- c(0, 1)
  toss <- sample(coin, size = ntrials, replace = TRUE, prob = c(1 - prob1, prob1))
  data <- data.frame(trial = 1:ntrials, toss = toss)
  write.csv(data, file_name, row.names = FALSE)
  return(data)
}

data <- generate_coin(prob1 = 0.70, ntrials = 2000)
head(data)
table(data$toss)
mean(data$toss)



# STEP 3: alpha
generate_coin_rw <- function(alpha = 0.1, V0 = 0.5, ntrials = 100, file_name = "simulation_data.csv") {
  coin <- c(0, 1)
  
  V <- numeric(ntrials + 1)
  V[1] <- V0
  toss <- numeric(ntrials)
  
  for (t in 1:ntrials) {
    toss[t] <- sample(coin, size = 1, prob = c(1 - V[t], V[t]))
    V[t + 1] <- V[t] + alpha * (toss[t] - V[t])
  }
  
  data <- data.frame(trial = 1:ntrials, toss = toss, V = V[1:ntrials])
  write.csv(data, file_name, row.names = FALSE)
  
  return(data)
}

setwd("/Users/sibrass/Documents/STAGE/scripts")
data <- generate_coin_rw(alpha = 0.1, V0 = 0.5, ntrials = 200)
head(data)

plot(data$trial, data$V)
plot(data$trial, data$toss)

# STAP 4: butterfly task
# taak waar subject 2 vlinders ziet (groen en blauw) waarbij eentje juist is (80% kans op reward) en eentje fout (20% kans op reward)
# model: V_stim(t+1) = V_stim(t) + alpha * (reward(t) - V_stim(t))
# keuze via softmax rule: P(green) = exp(beta * V_green) / (exp(beta * V_green) + exp(beta * V_blue))
# parameters: alpha (= learning rate, hoe hoger hoe sneller leren van prediction error) 
#             beta (= inverse temperature, hoe hoger hoe meer keuze gedreven is door values ipv random)

# HELPER FUNCTIONS
softmax <- function(V, beta) {
  exp(beta * V["green"]) /
    (exp(beta * V["green"]) + exp(beta * V["blue"]))
}
deliver_reward <- function(is_correct, p_reward_correct, p_reward_incorrect) {
  p_rew <- if (is_correct) p_reward_correct else p_reward_incorrect
  reward <- as.numeric(runif(1) < p_rew)
}
value_update <- function(reward, V, choice, alpha) {
  pred_error <- reward - V[choice]
  V[choice] <- V[choice] + alpha * pred_error
  V
}

# task maken
simulate_rw_task <- function(n_trials = 100,
                             correct_stim       = "green",
                             p_reward_correct   = 0.8,
                             p_reward_incorrect = 0.2,
                             alpha              = 0.2,
                             beta               = 3,
                             V_init             = c(green = 0, blue = 0)) {
  V <- V_init
  
  results <- data.frame(
    trial = 1:n_trials,
    choice = character(n_trials),
    correct = logical(n_trials),
    reward = numeric(n_trials), 
    V_green = numeric(n_trials), 
    V_blue = numeric(n_trials), 
    p_choose_green = numeric(n_trials)
  )
  
  for (t in 1:n_trials) {
    
    # softmax choice prob voor green
    p_green <- softmax(V, beta)
    
    # agent makes choice
    choice <- ifelse(runif(1) < p_green, "green", "blue")
    is_correct <- (choice == correct_stim)
    
    # reward delivery
    reward <- deliver_reward(is_correct, p_reward_correct, p_reward_incorrect)
    
    # actual RW value-update (of chosen option)
    V <- value_update(reward, V, choice, alpha)
    
    #log trial in eerder gemaakte database
    results$choice[t] <- choice
    results$correct[t] <- is_correct
    results$reward[t] <- reward
    results$V_green[t] <- V["green"]
    results$V_blue[t] <- V["blue"]
    results$p_choose_green[t] <- p_green
  }
  
  # voeg een attribuut "parameters" toe aan de results dataframe zodat exacte waarden van de huidig gerunde simulatie ook opgeslaan worden
  attr(results, "parameters") <- list(correct_stim = correct_stim, p_reward_correct = p_reward_correct, 
                                      p_reward_incorrect = p_reward_incorrect, alpha = alpha, beta = beta)
  results
}

#sim runnen
sim <- simulate_rw_task(
  n_trials = 100,
  correct_stim       = "green",
  p_reward_correct   = 0.6,
  p_reward_incorrect = 0.4,
  alpha              = 0.1,
  beta               = 3,
)

head(sim)

# plot maken
params <- attr(sim, "parameters")
plot(sim$trial, sim$V_green, type = "l", col = "forestgreen", lwd = "2", 
     ylim = c(0, 1), xlab = "Trial", ylab = "Associative Strength (V)", 
     main = "Rescorla-Wagner learning curves")
mtext(sprintf("alpha = %.2f, beta = %.1f", params$alpha, params$beta), side = 3, line = 0.3, cex = 0.85)
lines(sim$trial, sim$V_blue, col = "dodgerblue3", lwd = "2")
legend("bottomright", legend = c("Green butterfly", "Blue butterfly"), 
       col = c("forestgreen", "dodgerblue3"), lwd = "2", bty = "n")
