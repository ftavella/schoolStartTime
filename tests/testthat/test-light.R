twoDays <- seq(0, 48, length.out = 100)
rise <- tanh(0.6 * ((twoDays %% 24) - 8.0))
fall <- tanh(0.6 * ((twoDays %% 24) - 17.0))
expectedLightNoSchool <- (700.0 / 2.0) * (rise - fall) + 40.0
calculatedLightNoSchool <- sapply(twoDays, lightExposure, isAwake = TRUE,
                                  inSchool = FALSE, params = defaultParameters)
test_that(
  "lightExposure() works as expected when awake and not at school", {
    expect_equal(calculatedLightNoSchool, expectedLightNoSchool)
  }
)
expectedLightSchool <- rep(300.0, 100)
calculatedLightSchool <- sapply(twoDays, lightExposure, isAwake = TRUE,
                                inSchool = TRUE, params = defaultParameters)
test_that(
  "lightExposure() works as expected when awake and at school", {
    expect_equal(calculatedLightSchool, expectedLightSchool)
  }
)
expectedLightAsleep <- rep(0.0, 100)
calculatedLightAsleepSchool <- sapply(twoDays, lightExposure, isAwake = FALSE,
                                      inSchool = TRUE, params = defaultParameters)
calculatedLightAsleepNoSchool <- sapply(twoDays, lightExposure, isAwake = FALSE,
                                        inSchool = FALSE, params = defaultParameters)
test_that(
  "lightExposure() works as expected when student is asleep", {
    expect_equal(calculatedLightAsleepSchool, expectedLightAsleep)
    expect_equal(calculatedLightAsleepNoSchool, expectedLightAsleep)

  }
)
