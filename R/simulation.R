#' Simulates the circadian model.
#'
#' This function takes in an array of time values, an array of light values,
#' an initial condition, and a set of parameters. It uses the circadian model
#' to simulate the circadian rhythm over time.
#'
#' @param timeArray An array of time values.
#' @param lightArray An array of light values.
#' @param initialCondition The initial condition for the circadian model.
#' @param params A set of parameters for the circadian model.
#'
#' @return The output of the simulation.
#' @export
simulateCircadianModel <- function(timeArray, lightArray, initialCondition, params) {
  circadianWrapped <- function(t, x, params) {
    light <- stats::approx(x = timeArray, y = lightArray, xout = t, rule = 2)$y
    dC <- circadianModel(t, x, light, params)
    return(dC)
  }
  out <- deSolve::lsode(times = timeArray, y = initialCondition,
               func = circadianWrapped, parms = params)
  return(out)
}

#' simulateSleepModel Function
#'
#' This function simulates a sleep model using the LSODE solver.
#'
#' @param timeArray A numeric vector representing the time points for simulation.
#' @param wakeArray A numeric vector representing the wake values corresponding to the time points.
#' @param initialCondition A numeric vector representing the initial conditions for the sleep model.
#' @param params A list of parameters for the sleep model.
#'
#' @return A list containing the output of the LSODE solver.
#' @export
simulateSleepModel <- function(timeArray, wakeArray, initialCondition, params) {
  sleepWrapped <- function(t, x, params) {
    S <- stats::approx(x = timeArray, y = wakeArray, xout = t, rule = 2)$y
    dH <- sleepModel(t, x, S, params)
    return(dH)
  }
  out <- deSolve::lsode(times = timeArray, y = initialCondition,
                        func = sleepWrapped, parms = params)
  return(out)
}

#' Simulate Two Process Model
#'
#' This function simulates a two-process model using the LSODE solver.
#'
#' @param timeArray A numeric vector representing the time points at which the model will be evaluated.
#' @param lightArray A numeric vector representing the light values corresponding to each time point.
#' @param initialCondition A numeric vector representing the initial condition of the model.
#' @param params A list of parameters required for the model simulation.
#'
#' @return A list containing the simulation results.
#' @export
simulateTwoProcessModel <- function(timeArray, lightArray, initialCondition, params) {
  twoProcessWrapped <- function(t, x, params) {
    light <- stats::approx(x = timeArray, y = lightArray, xout = t, rule = 2)$y
    dTP <- twoProcessModel(t, x, light, params)
    return(dTP)
  }
  out <- deSolve::lsode(times = timeArray, y = initialCondition,
               func = twoProcessWrapped, parms = params,
               events = list(func = thresholdCrossingEvent, time = timeArray))
  return(out)
}
