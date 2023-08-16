library(deSolve)

# It's important that we have the importation of the library here, otherwise the rk4 function won't work

source(file.path("circadian_model.R"), local = TRUE)$value


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


R_plot <- ggscatter(data.frame(out), x="time", y="R")

#R_plot <- ggscatter(data, x="time", y="") for some other y value


