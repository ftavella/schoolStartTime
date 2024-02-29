## Test for getR1b function
test_that(
  "getR1b correctly calculates R1b value", {
    # Test with A = 1, R1tot = 2
    expect_equal(getR1b(1, 2),
                 0.5 * (1 + 2 + 4 - sqrt((1 + 2 + 4)^2 - 4 * 1 * 2)))
    # Test with A = 0, R1tot = 0
    expect_equal(getR1b(0, 0), 0)
    # Test with A = 2, R1tot = 3
    expect_equal(getR1b(2, 3),
                 0.5 * (2 + 3 + 4 - sqrt((2 + 3 + 4)^2 - 4 * 2 * 3)))
  })

## Test for sleepDrive function
params <- c(
  sleepDriveSlope = 3.25
)
R1BValues <- seq(0, 10, length.out = 100)
PsiValues <- seq(0, 2 * pi, length.out = 100)
expectedSleepDrive <- R1BValues - params[["sleepDriveSlope"]] * cos(PsiValues)
calculatedSleepDrive <- mapply(sleepDrive, R1BValues, PsiValues,
                               MoreArgs = list(params))
test_that("sleepDrive() returns the correct values", {
  expect_equal(calculatedSleepDrive, expectedSleepDrive)
})

## Test the sleepParameters() function
params <- c(
  awakeChi = 1,
  asleepChi = 2,
  awakeMu = 3,
  asleepMu = 4
)
awake_values <- sleepParameters(TRUE, params)
test_that("sleepParameters() returns the correct values for awake", {
  expect_equal(awake_values[1], 3)
  expect_equal(awake_values[2], 1)
})
asleep_values <- sleepParameters(FALSE, params)
test_that("sleepParameters() returns the correct values for asleep", {
  expect_equal(asleep_values[1], 4)
  expect_equal(asleep_values[2], 0.5)
})

## Test the sleepModel() function
params <- defaultParameters
t <- 0
x <- c(1, 1)
S <- 1
dxdt <- sleepModel(t, x, S, params)
test_that("sleepModel() returns the correct values", {
  expect_equal(dxdt[[1]][1], 47.772277228)
  expect_equal(dxdt[[1]][2], -0.00273583204)
})
