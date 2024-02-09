## Test for getAlpha function
test_that(
  "getAlpha correctly calculates alpha value", {
    # Define parameters
    params <- list("alpha_0" = 0.5, "P" = 2, "I_0" = 1)

    # Test with light intensity of 1
    expect_equal(getAlpha(1, params), 0.5 / 2)
    # Test with light intensity of 0
    expect_equal(getAlpha(0, params), 0)
    # Test with light intensity of 2
    expect_equal(getAlpha(2, params), 0.5 * 4 / (4 + 1))
  })

## Test for getBHat function
test_that(
  "getBHat correctly calculates BHat value", {
    # Define parameters
    params <- list("G" = 0.5, "alpha_0" = 0.5, "P" = 2, "I_0" = 1)

    # Test with n = 0.5 and light intensity of 1
    expect_equal(getBHat(0.5, 1, params), 0.5 * 0.5 * 0.5 / 2)
    # Test with n = 0 and light intensity of 0
    expect_equal(getBHat(0, 0, params), 0)
    # Test with n = 1 and light intensity of 2
    expect_equal(getBHat(1, 2, params), 0)
  })

## Test for phaseResponse function
test_that(
  "phaseResponse correctly calculates phase response value", {
    # Define parameters
    params <- list("A_1" = 0.5, "A_2" = 0.5, "sigma" = 1,
                   "beta_L1" = -0.0026, "beta_L2" = -0.957756)

    # Test with R = 1, Psi = 0
    expect_equal(phaseResponse(1.0, 0, params),
                 1 - 0.5 * 0.5 * 2 * sin(-0.0026) -
                   0.5 * 0.5 * 2 * sin(-0.957756))
    # Test with R = 2, Psi = pi/2
    expect_equal(phaseResponse(2, pi / 2, params),
                 1 - 0.5 * 0.5 * (8 + 0.5) * sin(pi / 2 - 0.0026) -
                   0.5 * 0.5 * (256 + 1) * sin(pi - 0.957756))
  })

## Test for amplitudeResponse function
test_that(
  "amplitudeResponse correctly calculates amplitude response", {
    # Define parameters
    params <- list("A_1" = 0.5, "A_2" = 0.5, "beta_L1" = -0.0026,
                   "beta_L2" = -0.957756)

    # Test with R = 1, Psi = 0
    expect_equal(amplitudeResponse(1, 0, params),
                 0.5 * 0.5 * (1 - 1) * cos(-0.0026) +
                   0.5 * 0.5 * 1 * (1 - 1) * cos(-0.957756))
    # Test with R = 0, Psi = 0
    expect_equal(amplitudeResponse(0, 0, params),
                 0.5 * 0.5 * (1 - 0) * cos(-0.0026) +
                   0.5 * 0.5 * 0 * (1 - 1) * cos(-0.957756))
    # Test with R = 2, Psi = pi/2
    expect_equal(amplitudeResponse(2, pi / 2, params),
                 0.5 * 0.5 * (1 - 16) * cos(pi / 2 - 0.0026) +
                   0.5 * 0.5 * 2 * (1 - 256) * cos(pi - 0.957756))
  })

## Test for circaModel function
# TODO: Write tests
