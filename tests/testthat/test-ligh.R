params <- c(
    lightSlope = 0.6,
    lightRiseTime = 8.0,
    lightFallTime = 17.0,
    lightAmplitude = 700.0,
    lightBaseline = 40.0,
    schoolBrightness = 300.0
)
twoDays <- seq(0, 48, length.out = 100)
rise <- tanh(0.6 * ((twoDays %% 24) - 8.0))
fall <- tanh(0.6 * ((twoDays %% 24) - 17.0))
expectedLightNoSchool <- (700.0 / 2.0) * (rise - fall) + 40.0
calculatedLightNoSchool <- sapply(twoDays, lightExposure, isAwake = TRUE,
                                  inSchool = FALSE, params = params)
test_that(
    "lightExposure() awake and no school", {
        expect_equal(calculatedLightNoSchool, expectedLightNoSchool)
    }
)
expectedLightSchool <- rep(300.0, 100)
calculatedLightSchool <- sapply(twoDays, lightExposure, isAwake = TRUE,
                                inSchool = TRUE, params = params)
test_that(
    "lightExposure() awake and in school", {
        expect_equal(calculatedLightSchool, expectedLightSchool)
    }
)
expectedLightAsleep <- rep(0.0, 100)
calculatedLightAsleepSchool <- sapply(twoDays, lightExposure, isAwake = FALSE,
                                      inSchool = TRUE, params = params)
calculatedLightAsleepNoSchool <- sapply(twoDays, lightExposure, isAwake = FALSE,
                                        inSchool = FALSE, params = params)
test_that(
    "lightExposure() asleep", {
        expect_equal(calculatedLightAsleepSchool, expectedLightAsleep)
        expect_equal(calculatedLightAsleepNoSchool, expectedLightAsleep)
    
    }
)