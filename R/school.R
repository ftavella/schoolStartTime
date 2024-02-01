#' Determine if a student is awake
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
isStudentAwake <- function(t, S, R1b, Psi, inSchool, params) {
    if (inSchool) {
        return(TRUE)
    }
    currentSleepDrive <- schoolStartTimes::sleepDrive(R1b, Psi, params)
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

#' Check if a given time is within a school day
#'
#' @param t Time in hours since the start of the week (0 = Sunday at midnight)
#' @return TRUE if t is within a school day (Monday to Friday), FALSE otherwise
#' @export
isSchoolDay <- function(t) {
    # A week in R starts on Sunday, which is day 0.
    # Therefore, Monday to Friday are days 1 to 5.
    ((t %/% 24) %% 7) < 5
}

#' Check if a given time is within school hours
#'
#' @param t Time in hours since the start of the week (0 = Sunday at midnight)
#' @param schoolStart Start of the school day in hours since midnight
#' @param schoolDuration Duration of the school day in hours
#' @return TRUE if t is within school hours, FALSE otherwise
#' @export
isWithinSchoolHours <- function(t, schoolStart, schoolDuration) {
    isAfterSchoolStart <- (t %% 24 >= schoolStart)
    schoolEndsAt <- schoolStart + schoolDuration
    isBeforeSchoolEnd <- (t %% 24 < schoolEndsAt)
    isAfterSchoolStart & isBeforeSchoolEnd
}

#' Check if a student is in school at a given time
#'
#' @param t Time in hours since the start of the week (0 = Sunday at midnight)
#' @param params A list containing 'schoolStart' and 'schoolDuration'
#' @return TRUE if a student is in school at time t, FALSE otherwise
#' @export
isStudentInSchool <- function(t, params) {
    schoolStart <- params[["schoolStart"]]
    schoolDuration <- params[["schoolDuration"]]
    inSchoolHours <- isWithinSchoolHours(t, schoolStart, schoolDuration)
    return(isSchoolDay(t) & inSchoolHours)
}