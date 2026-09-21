# GOAL: make new model more complex w prediction error sum
source("/Users/sibrass/Documents/STAGE/stage/functions.R")

impr_sim1 <- simulate_impr_switch()
par(mfrow = c(2, 1))
valueplot(impr_sim1)

impr_testsim1 <- simulate_impr_switch(alpha = 0.1, beta = 10, p_reward_correct = 1, p_reward_incorrect = 0)
par(mfrow = c(2, 1))
valueplot(impr_testsim1)
