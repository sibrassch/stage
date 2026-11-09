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

# load the data
data_file <- system.file("extdata", "bandit2arm_exampleData.txt", package = "hBayesDM")
raw <- read.table(data_file, header = TRUE)
head(raw)