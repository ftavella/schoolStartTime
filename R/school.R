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