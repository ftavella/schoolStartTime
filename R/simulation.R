simulateCircadianModel <- function(timeArray, lightArray, initialCondition, params) {
    circadianWrapped <- function(t, x, params) {
        light <- approx(x = timeArray, y = lightArray, xout = t, rule = 2)$y
        dC <- circadianModel(t, x, light, params)
        return(dC)
    }
    out <- lsode(times = timeArray, y = initialCondition,
                 func = circadianWrapped, parms = params)
    return(out)
}

simulateSleepModel <- function(timeArray, wakeArray, initialCondition, params) {
    sleepWrapped <- function(t, x, params) {
        S <- approx(x = timeArray, y = wakeArray, xout = t, rule = 2)$y
        dH <- sleepModel(t, x, S, params)
        return(dH)
    }
    out <- lsode(times = timeArray, y = initialCondition,
                 func = sleepWrapped, parms = params)
    return(out)
}

simulateTwoProcessModel <- function(timeArray, lightArray, initialCondition, params) {
    twoProcessWrapped <- function(t, x, params) {
        light <- approx(x = timeArray, y = lightArray, xout = t, rule = 2)$y
        dTP <- twoProcessModel(t, x, light, params)
        return(dTP)
    }
    out <- lsode(times = timeArray, y = initialCondition,
                func = twoProcessWrapped, parms = params,
                rootfun = distanceToThreshold,
                events = list(func = thresholdCrossingEvent, root = TRUE))
    return(out)
}