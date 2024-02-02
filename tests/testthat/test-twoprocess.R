## Test for getR1b function
test_that(
    "getR1b correctly calculates R1b value", {
    # Test with A = 1, R1tot = 2, Psi = 3
    expect_equal(getR1b(1, 2, 3),
                 0.5 * (1 + 2 + 4 - sqrt((1 + 2 + 4)^2 - 4 * 1 * 2)))
    # Test with A = 0, R1tot = 0, Psi = 0
    expect_equal(getR1b(0, 0, 0), 0)
    # Test with A = 2, R1tot = 3, Psi = 4
    expect_equal(getR1b(2, 3, 4),
                 0.5 * (2 + 3 + 4 - sqrt((2 + 3 + 4)^2 - 4 * 2 * 3)))
})

## Test for twoProcessModel function
# TODO: Write tests