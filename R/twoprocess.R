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

#' Model combining the circadian and sleep models
#'
#' @param t Time in hours since the start of the week (0 = Sunday at midnight)
#' @param x Current values of the model variables: R, Psi, N, A, R1tot, and S.
#' @param params A list of parameters used in the model.
#'
#' @return Derivatives of the model variables: dR, dPsi, dN, dA, dR1tot, and dS.
#' @export
twoProcessModel <- function(t, x, params) {
    # Wake state and light
    isAwake <- as.logical(x[6])
    inSchool <- isStudentInSchool(t, params)
    light <- lightExposure(t, isAwake, inSchool, params)
    # Circadian model
    dC <- circadianModel(t, x, light, params)
    dR <- dC[1]
    dPsi <- dC[2]
    dN <- dC[3]
    # Obtain current sleep parameters
    currentSleepParameters <- sleepParameters(t, isAwake, params)
    mu <- currentSleepParameters[1]
    ichi <- currentSleepParameters[2]
    # Sleep model
    dH <- sleepModel(t, x, mu, ichi, params)
    dA <- dH[1]
    dR1tot <- dH[2]
    # Return the right hand side of the ODEs
    return(list(c(dR, dPsi, dN, dA, dR1tot, 0)))
}