library(deSolve)
library(tidyverse)

source(file.path("src/circadian_model.R"), local = TRUE)$value

numberOfDays <- 49
dt<- 0.1
fullIntegrationWindow <- seq(from=0,to=numberOfDays * 24,by=dt)
  
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
  sleepDriveSlope = 3.25, # added value for sleepDriveSlope
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
  targc = 0.9677,
  lightSlope = 0.6,
  lightRiseTime = 8.0,
  lightFallTime = 17.0,
  lightAmplitude = 700.0,
  lightBaseline = 40.0)

default_initial_conditions <- c(R=0.8, Psi=2.5, n=0.8, A=767.7, R1tot=584.2)

out <- rk4(default_initial_conditions, fullIntegrationWindow, circadianModel, params)

R_plot <- ggplot(data.frame(out), aes(x=time, y=R)) + geom_line()
print(R_plot)
