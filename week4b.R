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