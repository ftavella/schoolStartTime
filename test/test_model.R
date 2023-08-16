library(deSolve)
library(tidyverse)

# ----------------------------

# Model parameters
Dwake <-555.4
Dsleep <- 572.7 
# TODO: add parameters
alpha0 <- 0.05
p <- 1.5
I0 <- 9325.0
Delta <- 0.0075
G <- 33.75

Irecep <- 1 / 291.0
Targc <- 0.9677
Tau <- 23.84
Beta1 <- -0.09318
A1    <- 0.3855
A2    <- 0.1977
BetaL1<- -0.0026 
BetaL2<- -0.957756
Sigma <- 0.0400692
K     <- 0.06458
Gamma <- 0.024


# Define parameters vector
sleepParameters <- c(Dwake, Dsleep)
lightParameters <- c(alpha0, p, I0, Delta, G)
circadianParameters <- c(Irecep, Targc, Tau, Beta1, A1, A2, BetaL1, BetaL2, Sigma, K, Gamma)
# Model initial conditions
# TODO: add initial conditions
# ...
default_initial_conditions <- c(R=0.8240484019863923, Psi=2.4532409247791804, n=0.78424752754260862, A=767.657, R1tot=584.24)

chiAwake <- 1.0/18.18
muAwake <- 869.5
chiAsleep <- 1.0/7.0
muAsleep <- 596.5
#----------------------------

# It's important that we have the importation of the library here, otherwise the rk4 function won't work

source(file.path("src/circadian_model.R"), local = TRUE)$value


numberOfDays <- 49
dt<- 0.1
fullIntegrationWindow <- seq(from=0,to=numberOfDays * 24,by=dt)
  
  
# allSchoolStartOptions <- c(5)  keeping this vector just in case 

params = c(
  awakeMu = 869.5,
  awakeChi = 18.18,
  asleepMu = 596.5,
  asleepChi = 7.0,
  schoolStart = 8.0,  # choose 8 for a more pragmatic persepctive
  socialFactor = 25.0,
  baselineLight = 30.0,
  wakeThreshold = 555.4,
  sleepThreshold = 572.7,
  schoolDuration = 7.0,
  schoolBrightness = 300,
  A_1 = 0.3855,
  A_2 = 0.1977,
  sigma = 0.0400692,
  beta_L1 = -0.0026,
  beta_L2 = -0.957756,
  P = 1.5,
  G = 33.75,
  I_0 = 9325.0,
  delta = 0.0075,
  alpha_0 = 0.05,
  K = 0.06458,
  tau = 23.84,
  gamma = 0.024,
  beta_1 = -0.09318,
  Irecep = 1.0 / 291.0,
  targc = 0.9677)


out <- rk4(default_initial_conditions, fullIntegrationWindow, circadianModel, params)

R_plot <- ggplot(data.frame(out), aes(x=time, y=R)) + geom_line()
print(R_plot)