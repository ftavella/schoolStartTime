## Test for twoProcessModel function
test_that(
    "twoProcessModel correctly calculates the right hand side of the ODEs", {
    t <- 0
    x <- c(0.5, 1, 0.5, 1, 1, 1)
    I <- 1
    dxdt <- twoProcessModel(t, x, I, defaultParameters)
    expect_equal(dxdt[[1]][1], 0.0030813912)
    expect_equal(dxdt[[1]][2], 0.260328966)
    expect_equal(dxdt[[1]][3], -0.224839159)
    expect_equal(dxdt[[1]][4], 47.772277228)
    expect_equal(dxdt[[1]][5], -0.00273583204)
    expect_equal(dxdt[[1]][6], 0)
})

## Test for sleepDrive function
test_that("sleepDrive correctly calculates the sleep drive", {
    R1b <- 1
    Psi <- 0
    params <- list(sleepDriveSlope = 1)
    drive <- sleepDrive(R1b, Psi, params)
    expect_equal(drive, 0)
})

## Test for nextWakeState function
test_that("nextWakeState is awake when in school", {
    t <- 0
    S <- 1
    R1b <- 2
    Psi <- 0
    inSchool <- TRUE
    params <- list(sleepThreshold = 2.5,
                   wakeThreshold = 1.5,
                   sleepDriveSlope = 1)
    next_state <- nextWakeState(t, S, R1b, Psi, inSchool, params)
    expect_true(next_state)
})
test_that("nextWakeState is awake below threshold", {
    t <- 0
    S <- 1
    R1b <- 2
    Psi <- 0 # drops below threshold
    inSchool <- FALSE
    params <- list(sleepThreshold = 2.5,
                   wakeThreshold = 1.5,
                   sleepDriveSlope = 1)
    next_state <- nextWakeState(t, S, R1b, Psi, inSchool, params)
    expect_true(next_state)
})
test_that("nextWakeState is asleep above threshold", {
    t <- 0
    S <- 1
    R1b <- 2
    Psi <- pi # rises above threshold
    inSchool <- FALSE
    params <- list(sleepThreshold = 2.5,
                   wakeThreshold = 1.5,
                   sleepDriveSlope = 1)
    next_state <- nextWakeState(t, S, R1b, Psi, inSchool, params)
    expect_false(next_state)
})
test_that("nextWakeState returns same state between thresholds", {
    # From awake
    t <- 0
    S <- 1
    R1b <- 2
    Psi <- pi/2 # in between thresholds
    inSchool <- FALSE
    params <- list(sleepThreshold = 2.5,
                   wakeThreshold = 1.5,
                   sleepDriveSlope = 1)
    next_state <- nextWakeState(t, S, R1b, Psi, inSchool, params)
    expect_equal(as.numeric(next_state), S)
    # From asleep
    S <- 0
    next_state <- nextWakeState(t, S, R1b, Psi, inSchool, params)
    expect_equal(as.numeric(next_state), S)
})

## Test for thresholdCrossingEvent
test_that("thresholdCrossingEvent returns awake when in school", {
    t <- 10
    S <- 1
    x <- c(0.5, 1, 0.5, 1, 1, S) # A = 6, R1tot = 4 => R1b = 2
    params <- list(sleepThreshold = 2.5,
                   wakeThreshold = 1.5,
                   sleepDriveSlope = 1,
                   schoolStart = 8,
                   schoolDuration = 7)
    new_x <- thresholdCrossingEvent(t, x, params)
    expect_equal(new_x[6], 1)
})
test_that("thresholdCrossingEvent returns awake when below threshold", {
    t <- 1 # Not in school
    S <- 0 # Asleep
    Psi <- 0
    x <- c(0.5, Psi, 0.5, 6, 4, S) # A = 6, R1tot = 4 => R1b = 2
    params <- list(sleepThreshold = 2.5,
                   wakeThreshold = 1.5,
                   sleepDriveSlope = 1,
                   schoolStart = 8,
                   schoolDuration = 7)
    new_x <- thresholdCrossingEvent(t, x, params)
    expect_equal(new_x[6], 1)
})
test_that("thresholdCrossingEvent returns asleep when above threshold", {
    t <- 1 # Not in school
    S <- 1 # Awake
    Psi <- pi
    x <- c(0.5, Psi, 0.5, 6, 4, S) # A = 6, R1tot = 4 => R1b = 2
    params <- list(sleepThreshold = 2.5,
                   wakeThreshold = 1.5,
                   sleepDriveSlope = 1,
                   schoolStart = 8,
                   schoolDuration = 7)
    new_x <- thresholdCrossingEvent(t, x, params)
    expect_equal(new_x[6], 0)
})
test_that("thresholdCrossingEvent: awake when above threshold and in school", {
    t <- 10 # In school
    S <- 1 # Awake
    Psi <- pi
    x <- c(0.5, Psi, 0.5, 6, 4, S) # A = 6, R1tot = 4 => R1b = 2
    params <- list(sleepThreshold = 2.5,
                   wakeThreshold = 1.5,
                   sleepDriveSlope = 1,
                   schoolStart = 8,
                   schoolDuration = 7)
    new_x <- thresholdCrossingEvent(t, x, params)
    expect_equal(new_x[6], 1)
})
test_that("thresholdCrossingEvent: same state if between thresholds ", {
    t <- 1 # Not in school
    S <- 1 # Awake
    Psi <- pi/2
    x <- c(0.5, Psi, 0.5, 6, 4, S) # A = 6, R1tot = 4 => R1b = 2
    params <- list(sleepThreshold = 2.5,
                   wakeThreshold = 1.5,
                   sleepDriveSlope = 1,
                   schoolStart = 8,
                   schoolDuration = 7)
    new_x <- thresholdCrossingEvent(t, x, params)
    expect_equal(new_x[6], S)

    S <- 0 # Asleep
    x <- c(0.5, Psi, 0.5, 6, 4, S) # A = 6, R1tot = 4 => R1b = 2
    new_x <- thresholdCrossingEvent(t, x, params)
    expect_equal(new_x[6], S)
})
test_that("thresholdCrossingEvent: the rest of the variables are unchanged", {
    t <- 10 # In school
    S <- 1 # Awake
    Psi <- pi
    x <- c(0.5, Psi, 0.5, 6, 4, S) # A = 6, R1tot = 4 => R1b = 2
    params <- list(sleepThreshold = 2.5,
                   wakeThreshold = 1.5,
                   sleepDriveSlope = 1,
                   schoolStart = 8,
                   schoolDuration = 7)
    new_x <- thresholdCrossingEvent(t, x, params)
    expect_equal(new_x[1], x[1])
    expect_equal(new_x[2], x[2])
    expect_equal(new_x[3], x[3])
    expect_equal(new_x[4], x[4])
    expect_equal(new_x[5], x[5])
})
