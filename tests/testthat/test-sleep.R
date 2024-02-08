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
# TODO: Write tests

## Test the sleepModel() function
# TODO: Write tests