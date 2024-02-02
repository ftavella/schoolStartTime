## Test the isStudentAwake function
params <- c(
    wakeThreshold = 555.4,
    sleepThreshold = 572.7,
    sleepDriveSlope = 3.25
  )
# If the student is in school, they are always awake
timeValues <- seq(0, 48, length.out = 100)
R1bValues <- seq(550, 580, length.out = 100)
PsiValues <- seq(0, 2 * pi, length.out = 100)
test_that(
    "Always awake in school", {
    expect_equal(
        sapply(R1bValues, isStudentAwake, t = 0, S = 1, Psi = pi,
               inSchool = TRUE, params = params),
        rep(TRUE, 100)
    )
    expect_equal(
        sapply(R1bValues, isStudentAwake, t = 0, S = 0, Psi = pi,
               inSchool = TRUE, params = params),
        rep(TRUE, 100)
    )
    expect_equal(
        sapply(PsiValues, isStudentAwake, t = 0, S = 1, R1b = 560,
               inSchool = TRUE, params = params),
        rep(TRUE, 100)
    )
    expect_equal(
        sapply(PsiValues, isStudentAwake, t = 0, S = 0, R1b = 560,
               inSchool = TRUE, params = params),
        rep(TRUE, 100)
    )
    expect_equal(
        sapply(timeValues, isStudentAwake, S = 1, R1b = 560,
               Psi = pi, inSchool = TRUE, params = params),
        rep(TRUE, 100)
    )
    expect_equal(
        sapply(timeValues, isStudentAwake, S = 0, R1b = 560,
               Psi = pi, inSchool = TRUE, params = params),
        rep(TRUE, 100)
    )
  }
)

# If sleep drive is below the wake threshold, the student is awake
R1b <- 550
Psi <- 0
test_that(
    "Awake when sleep drive is low", {
    expect_equal(
        isStudentAwake(0, 0, R1b, Psi, FALSE, params),
        TRUE
    )
    expect_equal(
        isStudentAwake(0, 1, R1b, Psi, FALSE, params),
        TRUE
    )
    expect_equal(
        isStudentAwake(16, 0, R1b, Psi, FALSE, params),
        TRUE
    )
    expect_equal(
        isStudentAwake(16, 1, R1b, Psi, FALSE, params),
        TRUE
    )
  }
)

# If sleep drive is above the sleep threshold, the student will fall asleep
R1b <- 580
Psi <- 0
test_that(
    "Asleep when sleep drive is high", {
    expect_equal(
        isStudentAwake(20.1, 0, R1b, Psi, FALSE, params),
        FALSE
    )
    expect_equal(
        isStudentAwake(21, 1, R1b, Psi, FALSE, params),
        FALSE
    )
    expect_equal(
        isStudentAwake(0, 0, R1b, Psi, FALSE, params),
        FALSE
    )
    expect_equal(
        isStudentAwake(16, 1, R1b, Psi, FALSE, params),
        FALSE
    )
  }
)

# Return the current state if the sleep drive is between the thresholds
R1b <- 565
Psi <- 0
test_that(
    "Return current state when sleep drive is between thresholds", {
    expect_equal(
        isStudentAwake(0, 0, R1b, Psi, FALSE, params),
        FALSE
    )
    expect_equal(
        isStudentAwake(0, 1, R1b, Psi, FALSE, params),
        TRUE
    )
    expect_equal(
        isStudentAwake(16, 0, R1b, Psi, FALSE, params),
        FALSE
    )
    expect_equal(
        isStudentAwake(16, 1, R1b, Psi, FALSE, params),
        TRUE
    )
  }
)

## Test the isSchoolDay function
test_that(
    "isSchoolDay correctly identifies school days", {
    # Monday (day 1)
    expect_true(isSchoolDay(24 - 12))
    # Tuesday (day 2)
    expect_true(isSchoolDay(48 - 12))
    # Wednesday (day 3)
    expect_true(isSchoolDay(72 - 12))
    # Thursday (day 4)
    expect_true(isSchoolDay(96 - 12))
    # Friday (day 5)
    expect_true(isSchoolDay(120 - 12))
    # Saturday (day 6)
    expect_false(isSchoolDay(144 - 12))
    # Sunday (day 0)
    expect_false(isSchoolDay(168 - 12))
})

test_that(
    "isSchoolDay correctly handles non-integer and negative inputs", {
    # Non-integer input
    expect_true(isSchoolDay(24.5))
})

## Test the isWithinSchoolHours function
test_that(
    "isWithinSchoolHours correctly identifies school hours", {
    # School starts at 9:00 and lasts for 8 hours
    schoolStart <- 9
    schoolDuration <- 8

    # Test a time during school hours
    expect_true(isWithinSchoolHours(10, schoolStart, schoolDuration))
    # Test a time before school starts
    expect_false(isWithinSchoolHours(8, schoolStart, schoolDuration))
    # Test a time after school ends
    expect_false(isWithinSchoolHours(18, schoolStart, schoolDuration))
    # Test the exact time school starts
    expect_true(isWithinSchoolHours(9, schoolStart, schoolDuration))
    # Test the exact time school ends
    expect_false(isWithinSchoolHours(17, schoolStart, schoolDuration))
})

## Test the isStudentInSchool function
test_that(
    "isStudentInSchool correctly identifies when a student is in school", {
    # School starts at 9:00 and lasts for 8 hours
    params <- list("schoolStart" = 9, "schoolDuration" = 8)

    # Test a time during school hours on a school day (Tuesday at 10:00)
    expect_true(isStudentInSchool(24 + 10, params))
    # Test a time before school starts on a school day (Tuesday at 8:00)
    expect_false(isStudentInSchool(24 + 8, params))
    # Test a time after school ends on a school day (Tuesday at 18:00)
    expect_false(isStudentInSchool(24 + 18, params))
    # Test a time during school hours on a non-school day (Saturday at 10:00)
    expect_false(isStudentInSchool(24 * 5 + 10, params))
})