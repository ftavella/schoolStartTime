#' Calculate the R1b value based on A, R1tot, and Psi
#'
#' @param A Numeric, the value of A
#' @param R1tot Numeric, the total value of R1
#'
#' @return Numeric, the calculated R1b value
#' @export
getR1b <- function(A, R1tot) {
    r1totSqrtTerm = sqrt((A + R1tot + 4.0) ^ 2 - 4 * A * R1tot)
    return(0.5 * (A + R1tot + 4 - r1totSqrtTerm))
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
#' @param S The current wakefulness state. 1 for awake, 0 for asleep.
#' @param params A list of additional parameters including Irecep and Targc.
#'
#' @return A vector containing the rate of change of A and R1tot.
#' @expor
sleepModel <- function(t, x, S, params) {
    # State variables
    A <- x[1]
    R1tot <- x[2]
    # Fixed parameters
    Irecep <- params[["Irecep"]]
    Targc <- params[["targc"]]
    # Dynamical parameters
    isAwake <- as.logical(S)
    currentSleepParameters <- sleepParameters(t, isAwake, params)
    mu <- currentSleepParameters[1]
    ichi <- currentSleepParameters[2]
    # Calculate the rate of change of A and R1tot
    R1b <- getR1b(A, R1tot)
    dA <- ichi * (mu - A)
    dR1tot <- Irecep * (R1b - Targc * R1tot)
    return(list(c(dA, dR1tot)))
}