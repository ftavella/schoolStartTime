#' Model combining the circadian and sleep models
#'
#' @param t Time in hours since the start of the week (0 = Sunday at midnight)
#' @param x Current values of the model variables: R, Psi, N, A, R1tot, and S.
#' @param light The light input to the model.
#' @param params A list of parameters used in the model.
#'
#' @return Derivatives of the model variables: dR, dPsi, dN, dA, dR1tot, and dS.
#' @export
twoProcessModel <- function(t, x, light, params) {
  R <- x[1]
  Psi <- x[2]
  n <- x[3]
  A <- x[4]
  R1tot <- x[5]
  S <- x[6]
  currentLight <- light * S
  # Circadian model
  xCircadian <- c(R, Psi, n)
  dC <- circadianModel(t, xCircadian, currentLight, params)
  dR <- dC[1]
  dPsi <- dC[2]
  dN <- dC[3]
  # Sleep model
  xSleep <- c(A, R1tot)
  dH <- sleepModel(t, xSleep, S, params)
  dA <- dH[1]
  dR1tot <- dH[2]
  # Return the right hand side of the ODEs
  return(list(c(dR, dPsi, dN, dA, dR1tot, 0)))
}

#' Calculate the sleep drive
#'
#' @param R1b The current value of R1b
#' @param Psi The current value of the Psi variable in the circadian model
#' @param params A list containing 'sleepDriveSlope'
#' @return The calculated sleep drive
#' @export
sleepDrive <- function(R1b, Psi, params) {
  slope <- params[["sleepDriveSlope"]]
  R1b - slope * cos(Psi)
}

#' Determine the next wake state of the student based on the current state
#'
#' @param t Time in hours since the start of the week (0 = Sunday at midnight)
#' @param S The current wake state. 1 if awake, 0 if asleep
#' @param R1b The current value of the R1b variable in the circadian model
#' @param Psi The current value of the Psi variable in the circadian model
#' @param inSchool TRUE if the student is currently in school, FALSE otherwise
#' @param params A parameter list
#' @return TRUE if the student is awake, FALSE otherwise
#' @details
#' Required parameters in `params` are:
#' - `sleepThreshold`: The sleep drive threshold for falling asleep
#' - `wakeThreshold`: The sleep drive threshold for waking up
#' @export
nextWakeState <- function(t, S, R1b, Psi, inSchool, params) {
  if (inSchool) {
    return(TRUE)
  }
  currentSleepDrive <- sleepDrive(R1b, Psi, params)
  sleepThreshold <- params[["sleepThreshold"]]
  wakeThreshold <- params[["wakeThreshold"]]
  if (currentSleepDrive < wakeThreshold) {
    return(TRUE)
  } else if (currentSleepDrive > sleepThreshold) {
    return(FALSE)
  } else {
    return(as.logical(S))
  }
}


#' Event function to detect when sleep or wake thresholds are crossed
#'
#' @param t Time in hours since the start of the week (0 = Sunday at midnight)
#' @param x The current state of the system.
#' @param params The parameters of the system.
#'
#' @return The state of the system with the wake state updated.
#' @export
thresholdCrossingEvent <- function(t, x, params) {
  R <- x[1]
  Psi <- x[2]
  n <- x[3]
  A <- x[4]
  R1tot <- x[5]
  S <- x[6]
  # Determine if sleep or wake thresholds are crossed
  R1b <- getR1b(A, R1tot)
  inSchool <- isStudentInSchool(t, params)
  isAwake <- nextWakeState(t, S, R1b, Psi, inSchool, params)
  newS <- as.numeric(isAwake)
  # Return the same state with the wake state updated
  return(c(R, Psi, n, A, R1tot, newS))
}

#' Function to calculate the distance to the wake and sleep thresholds
#'
#' @param t Time in hours since the start of the week (0 = Sunday at midnight)
#' @param x The current state of the system.
#' @param params The parameters of the system.
#'
#' @return Array with the distance to wake and sleep thresholds respectively.
#' @export
distanceToThreshold <- function(t, x, params) {
  Psi <- x[2]
  A <- x[4]
  R1tot <- x[5]
  sleepThreshold <- params[["sleepThreshold"]]
  wakeThreshold <- params[["wakeThreshold"]]
  R1b <- getR1b(A, R1tot)
  currentSleepDrive <- sleepDrive(R1b, Psi, params)
  wakeDistance <- currentSleepDrive - wakeThreshold
  sleepDistance <- currentSleepDrive - sleepThreshold
  return(c(wakeDistance, sleepDistance))
}
