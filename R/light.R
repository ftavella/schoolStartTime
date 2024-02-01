#' Calculate the student's light exposure for a given time
#'
#' @description
#' `lightExposure()` calculates the light exposure for a student at a given time
#' taking into account the time of day, whether the student is awake, and
#' whether the student is in school.
#'
#' @param t Time in hours since the start of the week (0 = Sunday at midnight)
#' @param isAwake A boolean indicating whether the student is awake
#' @param inSchool A boolean indicating whether the student is in school
#' @param params A list of parameters
#'
#' @details
#' Required parameters in `params` are:
#' - `lightSlope`: The slope of the light exposure curve (1/hours)
#' - `lightRiseTime`: The time of day when natural light exposure begins (hours)
#' - `lightFallTime`: The time of day when natural light exposure ends (hours)
#' - `lightAmplitude`: The amplitude of the light exposure curve (lux)
#' - `lightBaseline`: The baseline light exposure (lux)
#' - `schoolBrightness`: The light exposure in school (lux)
#'
#' @return The light exposure for the student as a scalar
#' @export
lightExposure <- function(t, isAwake, inSchool, params) {
    slope <- params[["lightSlope"]]
    riseTime <- params[["lightRiseTime"]]
    fallTime <- params[["lightFallTime"]]
    amplitude <- params[["lightAmplitude"]]
    baseline <- params[["lightBaseline"]]
    schoolBrightness <- params[["schoolBrightness"]]
    if (!isAwake) {
        # Asleep
        light <- 0.0
    } else if (inSchool) {
        # Awake and in school
        light <- schoolBrightness
    } else {
        # Awake and not in school
        rise <- tanh(slope * ((t %% 24) - riseTime))
        fall <- tanh(slope * ((t %% 24) - fallTime))
        smoothLight <- (amplitude / 2.0) * (rise - fall) + baseline
        light <- smoothLight
    }
    return(light)
}