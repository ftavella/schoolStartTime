#' Event function to detect when sleep or wake thresholds are crossed
#'
#' @param t The current time.
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
