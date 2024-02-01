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
