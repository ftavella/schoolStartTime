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
#' @param t Numeric, the current time
#' @param isAwake Logical, whether the student is awake.
#' @param inSchool Logical, whether the student is in school
#' @param params List, a list of parameters including:
#'   - awakeChi, asleepChi: Chi for awake and asleep states
#'   - awakeMu, asleepMu: Mu for awake and asleep states
#'
#' @return Numeric vector, the calculated mu, ichi, and light
sleepParameters <- function(t, isAwake, inSchool, params) {
    light <- lightExposure(t, isAwake, inSchool, params)
    ichi <- 1.0 / params[[if (isAwake) "awakeChi" else "asleepChi"]]
    mu <- params[[if (isAwake) "awakeMu" else "asleepMu"]]
    return(c(mu, ichi, light))
}