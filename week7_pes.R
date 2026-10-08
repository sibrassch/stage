# changing the prediction error sum to the rule used in the paper

# current rule: peS = peS + alphaPE * (pe_t - peS)

# new rule:     peS   = peS + alphaPE * (Npe - peS)
#               pe_t  = reward - unname(V_active[choice])
#               Npe   = min(pe_t, 0)                        # returns pe_t if negative, 0 otherwise


oldPESsim <- simulate_impr_switch(alpha = 0.1, 
                                  beta = 10, 
                                  alphaPE = 0.2, 
                                  pe_threshold = -0.2)

valueplot(oldPESsim)

# NEW SWITCH MODEL WITH NEW PES ===============================================================================================================================
simulate_switch2 <- function(n_trials            = 200,
                                 correct_stim        = "green",
                                 reversal_trial      = c(25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350, 375, 400),
                                 p_reward_correct    = 0.80,
                                 p_reward_incorrect  = 0.20,
                                 alpha               = 0.1,
                                 beta                = 10,
                                 alphaPE             = 0.2,
                                 pe_threshold        = -0.2,
                                 V_A_init            = c(green = 0.5, blue = 0.5),
                                 V_B_init            = c(green = 0.5, blue = 0.5),
                                 active_state_init   = "A",
                                 peS_init            = 0) {
  
  V_A                 <- V_A_init
  V_B                 <- V_B_init
  active_state        <- active_state_init
  trials_since_switch <- 0
  current_correct     <- correct_stim
  peS                 <- peS_init
  
  
  results <- data.frame(
    trial            = 1:n_trials,
    true_state       = character(n_trials),  # the actual state A or B of the task
    active_state     = character(n_trials),  # the state the model assumes it is in
    choice           = character(n_trials),
    correct          = logical(n_trials),
    reward           = numeric(n_trials),
    pe               = numeric(n_trials),
    peS              = numeric(n_trials),
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
    
    # raw prediction error, under current trial
    pe_t <- reward - unname(V_active[choice])
    
    # update values of the active state
    if (active_state == "A") {
      V_A <- value_update(V_A, choice, reward, alpha)
    } else {
      V_B <- value_update(V_B, choice, reward, alpha)
    }
    
    # running prediction error sum
    pe_neg <- min(pe_t, 0)                       
    peS    <- peS + alphaPE * (pe_neg - peS)     
    
    trials_since_switch <- trials_since_switch + 1
    
    switch_dec <- FALSE
    
    # trigger switch after prediction error sum reaches threshold
    if (peS < pe_threshold) {
      active_state          <- ifelse(active_state == "A", "B", "A")
      peS                   <- peS_init
      trials_since_switch   <- 0
      switch_dec            <- TRUE
    }
    
    # log everything in results
    results$true_state[t]       <- current_correct
    results$active_state[t]     <- active_state_used
    results$choice[t]           <- choice
    results$correct[t]          <- is_correct
    results$reward[t]           <- reward
    results$pe[t]               <- pe_t
    results$peS[t]              <- peS
    results$V_A_green[t]        <- V_A["green"]
    results$V_A_blue[t]         <- V_A["blue"]
    results$V_B_green[t]        <- V_B["green"]
    results$V_B_blue[t]         <- V_B["blue"]
    results$p_choose_green[t]   <- p_green
    results$switch_dec[t]       <- switch_dec
  }
  
  attr(results, "parameters") <- list(correct_stim = correct_stim, reversal_trial = reversal_trial,
                                      p_reward_correct = p_reward_correct, p_reward_incorrect = p_reward_incorrect, 
                                      alpha = alpha, beta = beta, alphaPE = alphaPE, pe_threshold = pe_threshold, 
                                      V_A_init = V_A_init, V_B_init = V_B_init, 
                                      active_state_init = active_state_init, peS_init = peS_init)
  results
}

newPESsim <- simulate_switch2(alpha = 0.1, 
                              beta = 10, 
                              alphaPE = 0.2, 
                              pe_threshold = -0.2)

valueplot(newPESsim)


set.seed(1); old <- simulate_impr_switch(alpha = 0.1, beta = 10, alphaPE = 0.2, pe_threshold = -0.2)
set.seed(1); new <- simulate_switch2(alpha = 0.1, beta = 10, alphaPE = 0.2, pe_threshold = -0.2)

c(old_switches = sum(old$switch_dec), new_switches = sum(new$switch_dec))
c(old_accuracy = mean(old$correct),   new_accuracy = mean(new$correct))

par(mfcol = c(2, 2))
valueplot(old)
valueplot(new)