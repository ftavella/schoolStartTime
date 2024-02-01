#' Calculate the alpha value based on the current light intensity
#'
#' @param light Numeric, the current light intensity
#' @param params List, a list of parameters including:
#'   - alpha_0: the maximum possible value of alpha
#'   - P: the exponent in the equation
#'   - I_0: the half-saturation constant
#'
#' @return Numeric, the calculated alpha value
#' @export
getAlpha <- function(light, params) {
    alpha0 <- params[["alpha_0"]]
    I0 <- params[["I_0"]]
    lightPowerP <- light ^ params[["P"]]
    return(alpha0 * lightPowerP / (lightPowerP + I0))
}

#' Calculate the BHat value based on n and light intensity
#'
#' @param n Numeric, the current value of n
#' @param light Numeric, the current light intensity
#' @param params List, a list of parameters including:
#'   - G: the maximum growth rate
#'   - alpha_0, P, I_0: parameters for the alpha calculation
#'
#' @return Numeric, the calculated BHat value
#' @export
getBHat <- function(n, light, params) {
    G <- params[["G"]]
    alpha <- getAlpha(light, params)
    return(G * (1.0 - n) * alpha)
}

#' Calculate the R1b value based on A, R1tot, and Psi
#'
#' @param A Numeric, the value of A
#' @param R1tot Numeric, the total value of R1
#' @param Psi Numeric, the value of Psi
#'
#' @return Numeric, the calculated R1b value
#' @export
getR1b <- function(A, R1tot, Psi) {
    r1totSqrtTerm = sqrt((A + R1tot + 4.0) ^ 2 - 4 * A * R1tot)
    return(0.5 * (A + R1tot + 4 - r1totSqrtTerm))
}

#' Calculate the phase response based on R and Psi
#'
#' @param R Numeric, the value of R
#' @param Psi Numeric, the value of Psi
#' @param params List, a list of parameters including:
#'   - A_1, A_2: coefficients for the first and second terms
#'   - sigma: the constant sigma
#'   - beta_L1, beta_L2: phase shifts for the first and second terms
#'
#' @return Numeric, the calculated phase response
#' @export
phaseResponse <- function(R, Psi, params) {
    A1 <- params[["A_1"]]
    A2 <- params[["A_2"]]
    Sigma <- params[["sigma"]]
    BetaL1 <- params[["beta_L1"]]
    BetaL2 <- params[["beta_L2"]]

    firstTerm <- A1 * 0.5 * (R^3 + 1.0 / R) * sin(Psi + BetaL1)
    secondTerm <- A2 * 0.5 * (1.0 + R^8) * sin(2.0 * Psi + BetaL2)
    return(Sigma - firstTerm - secondTerm)
}

#' Calculate the amplitude response based on R and Psi
#'
#' @param R Numeric, the value of R
#' @param Psi Numeric, the value of Psi
#' @param params List, a list of parameters including:
#'   - A_1, A_2: coefficients for the first and second terms
#'   - beta_L1, beta_L2: phase shifts for the first and second terms
#'
#' @return Numeric, the calculated amplitude response
#' @export
amplitudeResponse <- function(R, Psi, params) {
    A1 <- params[["A_1"]]
    A2 <- params[["A_2"]]
    BetaL1 <- params[["beta_L1"]]
    BetaL2 <- params[["beta_L2"]]

    firstTerm <- A1 * 0.5 * (1.0 - R^4) * cos(Psi + BetaL1)
    secondTerm <- A2 * 0.5 * R * (1.0 - R^8) * cos(2.0 * Psi + BetaL2)
    return(firstTerm + secondTerm)
}