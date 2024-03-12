#' Calculate the student's light exposure for a given time
#'
#' @description
#' `lightExposure()` calculates the light exposure for a student at a given time
#' taking into account the time of day, whether the student is awake, and
#' whether the student is in school.
#'
#' @param t Time in hours since the start of the week (0 = Sunday at midnight)
#' @param isAwake A Boolean indicating whether the student is awake
#' @param inSchool A Boolean indicating whether the student is in school
#' @param params A list of parameters. See below for details.
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

#' Calculate sunrise and sunset times for a given location and date
#'
#' @description
#' `sunriseSunset()` calculates the time of sunrise and sunset for a given
#' location and date. The function uses the `suncalc` package to calculate
#' the times based on the latitude and longitude of the location and the date.
#'
#' @param date A date object
#' @param latitude Latitude of the location
#' @param longitude Longitude of the location
#'
#' @return A list with two elements: `sunrise` and `sunset`, each containing
#' the time of the respective event as a floating point number between 0 and 24
#' @export
sunriseSunset <- function(date, latitude, longitude) {
  times <- suncalc::getSunlightTimes(date, latitude, longitude)
  sunriseUTC <- times$sunrise
  sunsetUTC <- times$sunset
  # Convert to local time
  localTZ <- lutz::tz_lookup_coords(latitude, longitude, 
                                    method = "fast", warn = FALSE)
  sunrise <- as.POSIXlt(lubridate::with_tz(sunriseUTC, tzone = localTZ))
  sunset <- as.POSIXlt(lubridate::with_tz(sunsetUTC, tzone = localTZ))
  sunriseTime <- sunrise$hour + sunrise$min / 60
  sunsetTime <- sunset$hour + sunset$min / 60
  return(list(sunrise = sunriseTime, sunset = sunsetTime))
}