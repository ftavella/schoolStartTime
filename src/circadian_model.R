library(deSolve)
library(tidyverse)

pow <- function(x, p){
    return (x ** p)
}

alpha <- function(light, params) {
    alpha0 <- params[["alpha_0"]]
    P <- params[["P"]]
    I0 <- params[["I_0"]]
    return (alpha0 * pow(light, P) / (pow(light, P) + I0))
}

bHat <- function(n, light, params) {
    G <- params[["G"]]
    return (G * (1.0 - n) * alpha(light, params))
}

typicalLight <- function(t, params){
    #TODO: create a function that returns 
    # the typical light intensity in a day
    # at a given time
    # t will come in as t %% 24
    light <- 0
    return (light)
}

phaseResponse <- function(R, Psi, params) {
    A1 <- params[["A_1"]]
    A2 <- params[["A_2"]]
    Sigma <- params[["sigma"]]
    BetaL1 <- params[["beta_L1"]]
    BetaL2 <- params[["beta_L2"]]

    firstTerm <- A1 * 0.5 * (pow(R, 3) + 1.0 / R) * sin(Psi + BetaL1)
    secondTerm <- A2 * 0.5 * (1.0 + pow(R, 8)) + sin(2.0 * Psi + BetaL2)
    return (Sigma - firstTerm - secondTerm)
}

amplitudeResponse <- function(R, Psi, params){
    A1 <- params[["A_1"]]
    A2 <- params[["A_2"]]
    BetaL1 <- params[["beta_L1"]]
    BetaL2 <- params[["beta_L2"]]

    firstTerm <- A1 * 0.5 * (1.0 - pow(R, 4)) * cos(Psi + BetaL1)
    secondTerm <- A2 * 0.5 * R * (1.0 - pow(R, 8)) * cos(2.0 * Psi + BetaL2)
    return(firstTerm + secondTerm)
}

sleepDrive <- function(R1b, Psi, params){
    sleepDriveSlope <- params[["sleepDriveSlope"]]
    return(R1b - sleepDriveSlope * cos(Psi))
}


sleepState <- function(t, A, R1tot, Psi, params) {
    # TODO: Migrate the previous code within circadianModel
    # to this function. This function determines whether
    # the student is awake or asleep at a given time.
    # returns the necessary parameters that circadianModel
    # needs to run the ODEs

    # 1. Determine if isAwake
    # set mu, ichi, and light based on isAwake
    mu = 0 # TODO: set mu properly
    ichi = 0 # TODO: set ichi properly
    light = 0 # TODO: set light properly

    return(c(mu, ichi, light))
}

circadianModel <- function(t, x, params){
    # State variables
    R <- x[1]
    Psi <- x[2]
    n <- x[3]
    A <- x[4]
    R1tot <- x[5]

    # Get R1b from R1tot and A
    r1totSqrtTerm = sqrt(pow(A + R1tot + 4.0, 2.0) - 4 * A * R1tot)
    R1b <- 0.5 * (A + R1tot + 4 - r1totSqrtTerm)

    # Determine if student is in school hours and if it is a school day
    schoolStart <- params[["schoolStart"]]
    schoolDuration <- params[["schoolDuration"]]
    gtSchoolStart <- (t %% 24 > schoolStart)
    schoolEnd <- schoolStart + schoolDuration
    lteSchoolEnd <- (t %% 24 <= schoolEnd)
    inSchoolHours <- gtSchoolStart && lteSchoolEnd
    isSchoolDay <- ((t %/% 24) %% 7) < 5


    # Determine if student is awake, and set mu, ichi, and light
    currentState <- sleepState(t, A, R1tot, Psi, params) 
    mu <- currentState[1]
    ichi <- currentState[2]
    light <- currentState[3]

    # Store lux value for plotting later, we'll work on how to get light info out later on
    # if (allLux[1 + floor(t / dt)] == -1) {
    #     allLux[1 + floor(t / dt)] <<- light
    # }

    # Calculate the right hand side of the ODEs
    K <- params[["K"]]
    Beta1 <- params[["beta_1"]]
    Tau <- params[["tau"]]
    Delta <- params[["delta"]]
    alpha_L <- alpha(light, params)
    BhatValue <- bHat(n, light, params)
    # Circadian model
    lightAmp <- BhatValue * amplitudeResponse(R, Psi, params)
    lightPhase <- BhatValue * phaseResponse(R, Psi, params)
    couplingTermAmp <- K * 0.5 * cos(Beta1) * R * (1.0 - pow(R, 4))
    dR = (-1.0 * Gamma * R) + couplingTermAmp + lightAmp
    naturalOscillation <- 2.0 * pi / Tau
    couplingTermPhase <- K / 2.0 * sin(Beta1) * (1 + pow(R, 4))
    dPsi <- naturalOscillation + couplingTermPhase + lightPhase
    dN <- 60.0 * (alpha_L * (1 - n) - Delta * n)

    # Sleep drive model
    Irecep <- params[["Irecep"]]
    Targc <- params[["targc"]]
    dA <- ichi * (mu - A)
    dR1tot <- Irecep * (R1b - Targc * R1tot)

    # Return the right hand side of the ODEs
    dxdt <- c(dR, dPsi, dN, dA, dR1tot)

    # Return results as a list
    return(list(dxdt))
}