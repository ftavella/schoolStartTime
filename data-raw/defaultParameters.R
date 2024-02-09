defaultParameters <- c(
  # Circadian model parameters
  A_1 = 0.3855, A_2 = 0.1977, sigma = 0.0400692,
  beta_L1 = -0.0026, beta_L2 = -0.957756,
  P = 1.5, G = 33.75, I_0 = 9325.0, delta = 0.0075, alpha_0 = 0.05, K = 0.06458,
  tau = 23.84, gamma = 0.024, beta_1 = -0.09318,
  # Sleep model parameters
  awakeMu = 869.5, awakeChi = 18.18, asleepMu = 596.5, asleepChi = 7.0,
  Irecep = 1.0 / 291.0, targc = 0.9677,
  # Two process model parameters
  wakeThreshold = 555.4, sleepThreshold = 572.7, sleepDriveSlope = 3.25,
  # School parameters
  schoolStart = 8.0, schoolDuration = 7.0,
  # Light parameters
  baselineLight = 30.0, schoolBrightness = 300,
  lightSlope = 0.6, lightRiseTime = 8.0, lightFallTime = 17.0,
  lightAmplitude = 700.0, lightBaseline = 40.0
)
usethis::use_data(defaultParameters, overwrite = TRUE)
