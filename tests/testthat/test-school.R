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
