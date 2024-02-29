## Test the simulateCircadianModel() function
test_that("Circadian model entrains to 24 hr period", {
    timeArray <- seq(0, 24 * 14 + 12, length.out = 5000)
    initialCondition <- c(R = 0.8, Psi = 2.5, n = 0.8)
    lightArray <- ifelse(timeArray %% 24 >= 8 & timeArray %% 24 <= 22, 1000, 0)
    result <- simulateCircadianModel(timeArray, lightArray, initialCondition,
                                    defaultParameters)
    data <- data.frame(result)
    # get only data from the last 7 days
    data <- data[data$time > 7 * 24, ]
    time <- data$time
    state <- data$R * cos(data$Psi)
    # find zero crossings in the state
    crossings <- which(diff(sign(state)) != 0)
    # roughly calculate the mean period
    crossing_durations <- diff(time[crossings])
    periods <- crossing_durations[seq(1, length(crossing_durations), 2)] +
    crossing_durations[seq(2, length(crossing_durations), 2)]
    period <- mean(periods)
    expect_true(period - 24.0 < 1e-2)
})

## Test the simulateSleepModel() function
test_that("Sleep model has the correct behavior", {
    timeArray <- seq(0, 72, length.out = 1000)
    initialCondition <- c(A = 767.7, R1tot = 584.2)
    wakeArray <- ifelse(timeArray %% 24 >= 8 & timeArray %% 24 <= 22, 1, 0)
    result <- simulateSleepModel(timeArray, wakeArray, initialCondition,
                                defaultParameters)
    data <- data.frame(result)
    A <- data$A
    awake_A_diff <- diff(A[wakeArray == 1])
    asleep_A_diff <- diff(A[wakeArray == 0])
    # remove very large values (which are due to disconnected points)
    awake_A_diff <- awake_A_diff[awake_A_diff > -100]
    asleep_A_diff <- asleep_A_diff[asleep_A_diff < 100]
    # when wakeArray is 1, A should have a positive slope
    expect_true(all(awake_A_diff > 0))
    # when wakeArray is 0, A should have a negative slope
    expect_true(all(asleep_A_diff < 0))
})

## Test the simulateTwoProcessModel() function
test_that("Two-process model behaves correctly", {
    timeArray <- seq(0, 24 * 14 + 12, length.out = 2000)
    initialCondition <- c(R = 0.8, Psi = 2.5, n = 0.8,
                        A = 760, R1tot = 580, S = 0)
    lightArray <- ifelse(timeArray %% 24 >= 8 & timeArray %% 24 <= 22, 1000, 0)
    result <- simulateTwoProcessModel(timeArray, lightArray, initialCondition,
                                    defaultParameters)
    data <- data.frame(result)
    # Sleep drive should increase when awake and decrease when asleep
    R1b <- getR1b(data$A, data$R1tot)
    sleep_drive <- sleepDrive(R1b, data$Psi, defaultParameters)
    awake_sleep_drive_diff <- diff(sleep_drive[data$S == 1])
    asleep_sleep_drive_diff <- diff(sleep_drive[data$S == 0])
    # remove very large values (which are due to disconnected points)
    awake_sleep_drive_diff <- awake_sleep_drive_diff[awake_sleep_drive_diff > -5]
    asleep_sleep_drive_diff <- asleep_sleep_drive_diff[asleep_sleep_drive_diff < 5]
    # when S is 1, sleep drive should increase
    expect_true(all(awake_sleep_drive_diff > 0))
    # when S is 0, sleep drive should decrease
    expect_true(all(asleep_sleep_drive_diff < 0))
    # Student should be always awake during school time
    school_time <- isStudentInSchool(data$time, defaultParameters)
    # shift school time 1 samples forward because
    # events are calculated at the end of the time step
    school_time <- c(FALSE, head(school_time, -1))
    expect_true(all(data$S[school_time] > 0))
    # Entrains correctly to 24 hr period
    data <- data[data$time > 7 * 24, ]
    time <- data$time
    state <- data$R * cos(data$Psi)
    # find zero crossings in the state
    crossings <- which(diff(sign(state)) != 0)
    # roughly calculate the mean period
    crossing_durations <- diff(time[crossings])
    periods <- crossing_durations[seq(1, length(crossing_durations), 2)] +
    crossing_durations[seq(2, length(crossing_durations), 2)]
    period <- mean(periods)
    expect_true(period - 24.0 < 1e-2)
})