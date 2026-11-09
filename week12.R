library(hBayesDM)
library(cmdstanr)

fit <- bandit2arm_delta(data = "example", niter = 2000, nwarmup = 1000,
                        nchain = 4, ncore = 4)
plot(fit)


fit2 <- bandit2arm_delta(data = "example", niter = 3000, nwarmup = 1000,
                        nchain = 5, ncore = 5)
plot(fit2)

## ATTEMPT: fitting own RW model to "example" data from hBayesDM package
library(cmdstanr)
library(posterior)
library(loo)

# STEP 1: load the data
data_file <- system.file("extdata", "bandit2arm_exampleData.txt", package = "hBayesDM")
raw <- read.table(data_file, header = TRUE)
head(raw)

raw$reward <- as.integer(raw$outcome > 0)

table(raw$choice)
table(raw$reward)

# STEP 2: turn into list
raw         <- raw[order(raw$subjID, raw$trial), ]
subjects    <- unique(raw$subjID)
N           <- length(subjects)
Tsubj       <- as.integer(table(factor(raw$subjID, levels = subjects)))
Tmax        <- max(Tsubj)

choice_mat <- matrix(0L, N, Tmax)
reward_mat <- matrix(0L, N, Tmax)
for (i in seq_len(N)) {
  d <- raw[raw$subjID == subjects[i], ]
  choice_mat[i, 1:Tsubj[i]] <- d$choice
  reward_mat[i, 1:Tsubj[i]] <- d$reward
}

stan_data <- list(N = N, T = Tmax, Tsubj = Tsubj,
                  choice = choice_mat, reward = reward_mat)
str(stan_data)

# STEP 3: fit the RW model (in stan code)
rw_code <-  "

data {
  int<lower=1> N;
  int<lower=1> T;
  array[N] int<lower=1, upper=T> Tsubj;
  array[N, T] int<lower=0, upper=2> choice;
  array[N, T] int<lower=0, upper=1> reward;
}

parameters {
  vector[2] mu_pr;
  vector<lower=0>[2] sigma;
  vector[N] alpha_pr;
  vector[N] beta_pr;
}

transformed parameters {
  vector<lower=0, upper=1>[N]   alpha = Phi_approx(mu_pr[1] + sigma[1] * alpha_pr);
  vector<lower=0, upper=20>[N]  beta  = 20 * Phi_approx(mu_pr[2] + sigma[2] * beta_pr);
}

model {
  mu_pr     ~ normal(0, 1);
  sigma     ~ normal(0, 0.2);
  alpha_pr  ~ normal(0, 1);
  beta_pr   ~ normal(0, 1);
  
  for (i in 1:N) {
    vector[2] V = rep_vector(0.5, 2);
    for (t in 1:Tsubj[i]) {
      choice[i, t] ~ categorical_logit(beta[i] * V);
      V[choice[i, t]] += alpha[i] * (reward[i, t] - V[choice[i, t]]);
    }
  }
}

generated quantities {
  real<lower=0, upper=1> mu_alpha = Phi_approx(mu_pr[1]);
  real<lower=0, upper=20> mu_beta = 20 * Phi_approx(mu_pr[2]);
  
  vector[N] log_lik = rep_vector(0, N);
  array[N, T] int y_pred = rep_array(-1, N, T);
  
  for (i in 1:N) {
    vector[2] V = rep_vector(0.5, 2);
    for (t in 1:Tsubj[i]) {
      log_lik[i]        +=  categorical_logit_lpmf(choice[i, t] | beta[i] * V);
      y_pred[i, t]      =   categorical_logit_rng(beta[i] * V);
      V[choice[i, t]]   +=  alpha[i] * (reward[i, t] - V[choice[i, t]]);
    }
  }
}
"

mod <-  cmdstan_model(write_stan_file(rw_code))

fit_own <- mod$sample(data = stan_data, chains = 4, parallel_chains = 4,
                      iter_warmup = 1000, iter_sampling = 1000, seed = 123)

# STEP 4: convergence check
fit_own$diagnostic_summary()
s <-  fit_own$summary(c("mu_alpha", "mu_beta", "sigma", "alpha", "beta"))
max(s$rhat) # limit would be 1.01

# STEP 5: results
# 5a: group level
fit_own$summary(c("mu_alpha", "mu_beta", "sigma"))

group <- fit_own$draws(c("mu_alpha", "mu_beta"), format = "draws_df")
par(mfrow= c(1, 2))
hist(group$mu_alpha, breaks = 40, freq = FALSE, 
     main = "Group learning rate", xlab = "mu_alpha")
hist(group$mu_beta, breaks = 40, freq = FALSE, 
     main = "Group inverse temperature", xlab = "mu_beta")

# 5b: individual level (posterior mean)
alpha_draws <- fit_own$draws("alpha", format = "draws_matrix")   # 4000 x N
beta_draws  <- fit_own$draws("beta",  format = "draws_matrix")
ind_pars <- data.frame(
  subjID     = subjects,
  alpha_mean = colMeans(alpha_draws),
  alpha_lo   = apply(alpha_draws, 2, quantile, 0.025),
  alpha_hi   = apply(alpha_draws, 2, quantile, 0.975),
  beta_mean  = colMeans(beta_draws),
  beta_lo    = apply(beta_draws, 2, quantile, 0.025),
  beta_hi    = apply(beta_draws, 2, quantile, 0.975)
)
print(ind_pars, digits = 3)

# 5c: maximum likelihood per participant
rw_nll <- function(par, choice, reward) {
  alpha <- plogis(par[1])
  beta  <- exp(par[2])
  V <- c(0.5, 0.5)
  nll <- 0
  for (t in seq_along(choice)) {
    p <- exp(beta * V) / sum(exp(beta * V))
    nll <- nll - log(p[choice[t]])
    V[choice[t]] <- V[choice[t]] + alpha * (reward[t] - V[choice[t]])
  }
  nll
}

fit_ml <- function(choice, reward, n_starts = 10) {
  best <- NULL
  for (s in 1:n_starts) { 
    start <- c(qlogis(runif(1, 0.05, 0.9)), log(runif(1, 0.5, 15)))
    o <- optim(start, rw_nll, choice = choice, reward = reward)
    if (is.null(best) || o$value < best$value) best <- o
  }
  c(alpha_ml = plogis(best$par[1]),
    beta_ml  = exp(best$par[2]),
    nll      = best$value,
    bic      = 2 * best$value + 2 * log(length(choice)))
}

set.seed(123)
ml <- t(sapply(seq_len(N), function(i) {
  fit_ml(choice_mat[i, 1:Tsubj[i]], reward_mat[i, 1:Tsubj[i]])
}))
ind_pars <- cbind(ind_pars, ml)
print(ind_pars, digits = 3)

# vergelijking bayesian vs maximum likelihood (SHRINKAGE!!)
par(mfrow = c(1, 2))
lim_a <- range(ind_pars$alpha_ml, ind_pars$alpha_lo, ind_pars$alpha_hi)
plot(ind_pars$alpha_ml, ind_pars$alpha_mean, pch = 16, xlim = lim_a, ylim = lim_a,
     xlab = "ML estimate", ylab = "Bayesian posterior mean (95% CI)",
     main = "Learning rate alpha")
arrows(ind_pars$alpha_ml, ind_pars$alpha_lo, ind_pars$alpha_ml, ind_pars$alpha_hi,
       length = 0, col = "grey60")
abline(0, 1, lty = 2)

lim_b <- range(ind_pars$beta_ml, ind_pars$beta_lo, ind_pars$beta_hi)
plot(ind_pars$beta_ml, ind_pars$beta_mean, pch = 16, xlim = lim_b, ylim = lim_b,
     xlab = "ML estimate", ylab = "Bayesian posterior mean (95% CI)",
     main = "Inverse temperature beta")
arrows(ind_pars$beta_ml, ind_pars$beta_lo, ind_pars$beta_ml, ind_pars$beta_hi,
       length = 0, col = "grey60")
abline(0, 1, lty = 2)
par(mfrow = c(1, 1))

round(rbind(
  ML       = c(sd_alpha = sd(ind_pars$alpha_ml),   sd_beta = sd(ind_pars$beta_ml)),
  Bayesian = c(sd_alpha = sd(ind_pars$alpha_mean), sd_beta = sd(ind_pars$beta_mean))
), 3)

# STEP 6: vergelijking met de hbaysdm bandit2armdelta
library(hBayesDM)
fit_hb <- bandit2arm_delta(data = "example", niter = 2000, nwarmup = 1000,
                           nchain = 4, ncore = 4)
hb <- fit_hb$all_ind_pars

par(mfrow = c(1, 2))
plot(hb$A, ind_pars$alpha_mean, pch = 16,
     xlab = "hBayesDM A", ylab = "my model: alpha", main = "Learning rate")
abline(0, 1, lty = 2)
plot(hb$tau, ind_pars$beta_mean, pch = 16,
     xlab = "hBayesDM tau", ylab = "my model: beta", main = "Inverse temperature")
abline(0, 2, lty = 2)          
par(mfrow = c(1, 1))