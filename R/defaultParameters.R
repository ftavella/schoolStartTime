#' Default parameters for the several models and functions in the package
#'
#' The named vector contains parameters for the circadian, sleep, and two
#' process models plus parameters for the light functions.
#'
#' @format A named vector with the following parameters
#' \describe{
#'   \item{A_1}{Circadian parameter}
#'   \item{A_2}{Circadian parameter}
#'   \item{sigma}{Circadian parameter}
#'   \item{beta_L1}{Circadian parameter}
#'   \item{beta_L2}{Circadian parameter}
#'   \item{P}{Circadian parameter}
#'   \item{G}{Circadian parameter}
#'   \item{I_0}{Circadian parameter}
#'   \item{delta}{Circadian parameter}
#'   \item{alpha_0}{Circadian parameter}
#'   \item{K}{Circadian parameter}
#'   \item{tau}{Circadian parameter}
#'   \item{gamma}{Circadian parameter}
#'   \item{beta_1}{Circadian parameter}
#'   \item{awakeMu}{Sleep parameter}
#'   \item{awakeChi}{Sleep parameter}
#'   \item{asleepMu}{Sleep parameter}
#'   \item{asleepChi}{Sleep parameter}
#'   \item{Irecep}{Sleep parameter}
#'   \item{targc}{Sleep parameter}
#'   \item{wakeThreshold}{Two process parameter}
#'   \item{sleepThreshold}{Two process parameter}
#'   \item{sleepDriveSlope}{Two process parameter}
#'   \item{schoolStart}{School parameter}
#'   \item{schoolDuration}{School parameter}
#'   \item{baselineLight}{Light parameter}
#'   \item{schoolBrightness}{Light parameter}
#'   \item{lightSlope}{Light parameter}
#'   \item{lightRiseTime}{Light parameter}
#'   \item{lightFallTime}{Light parameter}
#'   \item{lightAmplitude}{Light parameter}
#'   \item{lightBaseline}{Light parameter}
#' }
#'
#' @source Arcascope
"defaultParameters"