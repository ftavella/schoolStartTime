# TODO: Create parameter arrays for each test
#       with only the relevant parameters to
#       each function
params <- c(
  awakeMu = 869.5,
  awakeChi = 18.18,
  asleepMu = 596.5,
  asleepChi = 7.0,
  schoolStart = 8.0,
  socialFactor = 25.0,
  baselineLight = 30.0,
  wakeThreshold = 555.4,
  sleepThreshold = 572.7,
  schoolDuration = 7.0,
  schoolBrightness = 300,
  sleepDriveSlope = 3.25,
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

## Test the simulateCircadianModel() function
timeArray <- seq(0, 72, length.out = 1000)
initialCondition <- c(R = 0.8, Psi = 2.5, n = 0.8)
lightArray <- ifelse(timeArray %% 24 >= 8 & timeArray %% 24 <= 22, 1000, 0)
result <- simulateCircadianModel(timeArray, lightArray, initialCondition,
                                 params)
data <- data.frame(result)
# TODO: Test that period converges
# TODO: Test simulation in the darkness

## Test the simulateSleepModel() function
timeArray <- seq(0, 72, length.out = 1000)
initialCondition <- c(A = 767.7, R1tot = 584.2)
wakeArray <- ifelse(timeArray %% 24 >= 8 & timeArray %% 24 <= 22, 1, 0)
result <- simulateSleepModel(timeArray, wakeArray, initialCondition, params)
data <- data.frame(result)
# TODO: Tests that when the wakeArray is 1,
#       variable A has a positive slope and when it is 0,
#       A has a negative slope


## Test the eventfunc() function
# TODO: Write tests

## Test the rootfunc() function
# TODO: Write tests

## Test the simulateTwoProcessModel() function
# TODO: Write tests
timeArray <- seq(0, 144, length.out = 5000)
initialCondition <- c(R = 0.8, Psi = 2.5, n = 0.8,
                      A = 760, R1tot = 580, S = 0)
lightArray <- ifelse(timeArray %% 24 >= 8 & timeArray %% 24 <= 22, 1000, 0)
result <- simulateTwoProcessModel(timeArray, lightArray, initialCondition,
                                  params)
# data <- data.frame(result)
# R1b <- getR1b(data$A, data$R1tot)
# drive <- sleepDrive(R1b, data$Psi, params)
# plot(timeArray, drive, type = "l")

# attributes(result)$troot
