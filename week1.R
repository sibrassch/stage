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

setwd("/Users/sibrass/Documents/STAGE")
data <- generate_coin_rw(alpha = 0.1, V0 = 0.5, ntrials = 200)
head(data)
