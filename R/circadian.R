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

#' Calculate dR, dPsi, and dN based on the Hannay 19 et al. model
#'
#' @param t Time in hours since the start of the week (0 = Sunday at midnight)
#' @param x Current values of the circadian model variables: R, Psi, and n.
#' @param light The light input to the circadian model.
#' @param params A list of parameters used in the circadian model.
#'
#' @return Derivatives of the circadian model variables: dR, dPsi, and dN.
#' @export
circadianModel <- function(t, x, light, params) {
    R <- x[1]
    Psi <- x[2]
    n <- x[3]
    K <- params[["K"]]
    Beta1 <- params[["beta_1"]]
    Tau <- params[["tau"]]
    Delta <- params[["delta"]]
    Gamma <- params[["gamma"]]
    alpha_L <- getAlpha(light, params)
    Bhat <- getBHat(n, light, params)
    lightAmp <- Bhat * amplitudeResponse(R, Psi, params)
    lightPhase <- Bhat * phaseResponse(R, Psi, params)
    couplingTermAmp <- K * 0.5 * cos(Beta1) * R * (1.0 - R^4)
    dR = (-1.0 * Gamma * R) + couplingTermAmp + lightAmp
    naturalOscillation <- 2.0 * pi / Tau
    couplingTermPhase <- K / 2.0 * sin(Beta1) * (1 + R^4)
    dPsi <- naturalOscillation + couplingTermPhase + lightPhase
    dN <- 60.0 * (alpha_L * (1 - n) - Delta * n)
    return(c(dR, dPsi, dN))
}