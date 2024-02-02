#' Calculate the sleep drive
#'
#' @param R1b The current value of the R1b variable in the circadian model
#' @param Psi The current value of the Psi variable in the circadian model
#' @param params A list containing 'sleepDriveSlope'
#' @return The calculated sleep drive
#' @export
sleepDrive <- function(R1b, Psi, params) {
    slope <- params[["sleepDriveSlope"]]
    R1b - slope * cos(Psi)
}

#' Calculate the sleep parameters for the current wakefulness state
#'
#' @param t Time in hours since the start of the week (0 = Sunday at midnight)
#' @param isAwake Logical, whether the student is awake.
#' @param params List, a list of parameters including:
#'   - awakeChi, asleepChi: Chi for awake and asleep states
#'   - awakeMu, asleepMu: Mu for awake and asleep states
#'
#' @return Numeric vector, the calculated mu, ichi, and light
sleepParameters <- function(t, isAwake, params) {
    ichi <- 1.0 / params[[if (isAwake) "awakeChi" else "asleepChi"]]
    mu <- params[[if (isAwake) "awakeMu" else "asleepMu"]]
    return(c(mu, ichi))
}

#' Calculates the rate of change of A and R1tot
#'
#' @param t Time in hours since the start of the week (0 = Sunday at midnight)
#' @param x Current state
#' @param mu ODE parameter
#' @param ichi ODE parameter
#' @param params A list of additional parameters including Irecep and Targc.
#'
#' @return A vector containing the rate of change of A and R1tot.
#' @export
sleepModel <- function(t, x, mu, ichi, params) {
    Psi <- x[2]
    A <- x[4]
    R1tot <- x[5]
    Irecep <- params[["Irecep"]]
    Targc <- params[["targc"]]
    R1b <- getR1b(A, R1tot, Psi)
    dA <- ichi * (mu - A)
    dR1tot <- Irecep * (R1b - Targc * R1tot)
    return(c(dA, dR1tot))
}