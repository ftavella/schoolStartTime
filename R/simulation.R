#' Event function to detect when sleep or wake thresholds are crossed
#'
#' @param t Time in hours since the start of the week (0 = Sunday at midnight)
#' @param x The current state of the system.
#' @param params The parameters of the system.
#'
#' @return The state of the system with the wake state updated.
#' @export
eventfunc <- function(t, x, params) {
    R <- x[1]
    Psi <- x[2]
    n <- x[3]
    A <- x[4]
    R1tot <- x[5]
    S <- x[6]
    # Determine if sleep or wake thresholds are crossed
    R1b <- getR1b(A, R1tot, Psi)
    inSchool <- isStudentInSchool(t, params)
    isAwake <- isStudentAwake(t, S, R1b, Psi, inSchool, params)
    newS <- as.numeric(isAwake)
    # Return the same state with the wake state updated
    return(c(R, Psi, n, A, R1tot, newS))
}

#' Root function to detect when sleep or wake thresholds are crossed
#' 
#' @param t Time in hours since the start of the week (0 = Sunday at midnight)
#' @param x The current state of the system.
#' @param params The parameters of the system.
#' 
#' @return Array with the distance to wake and sleep thresholds respectively.
#' @export
rootfunc <- function(t, x, params) {
    Psi <- x[2]
    A <- x[4]
    R1tot <- x[5]
    sleepThreshold <- params[["sleepThreshold"]]
    wakeThreshold <- params[["wakeThreshold"]]
    R1b <- getR1b(A, R1tot, Psi)
    currentSleepDrive <- sleepDrive(R1b, Psi, params)
    wakeDistance <- abs(currentSleepDrive - wakeThreshold)
    sleepDistance <- abs(currentSleepDrive - sleepThreshold)
    return(c(wakeDistance, sleepDistance))
}

simulateTwoProcessModel <- function(timeArray, initialCondition, params) {
    # TODO: Add a way of recording light information out of simulation
    out <- lsode(times = timeArray, y = initialCondition,
                func = twoProcessModel, parms = params,
                rootfun = rootfunc,
                events = list(func = eventfunc, root = TRUE))
    return(out)
}

simulateCircadianModel <- function(timeArray, lightArray, initialCondition, params) {
    circadianWrapped <- function(t, x, params) {
        light <- approx(x = timeArray, y = lightArray, xout = t)$y
        dC <- circadianModel(t, x, light, params)
        return(dC)
    }

    out <- lsode(times = timeArray, y = initialCondition,
                 func = circadianWrapped, parms = params)
    return(out)
}

simulateSleepModel <- function(timeArray, initialCondition, params) {
    out <- lsode(times = timeArray, y = initialCondition,
                 func = sleepModel, parms = params)
    return(out)
}