library(deSolve)
library(tidyverse)

pow <- function(x, p){
    return(x ** p)
}

alpha <- function(light, params) {
    alpha0 <- params[["alpha_0"]]
    P <- params[["P"]]
    I0 <- params[["I_0"]]
    return(alpha0 * pow(light, P) / (pow(light, P) + I0))
}

bHat <- function(n, light, params) {
    G <- params[["G"]]
    return(G * (1.0 - n) * alpha(light, params))
}

sleepDrive <- function(R1bValue, Psi, params){
    sleepDriveSlope <- params[["sleepDriveSlope"]]
    return(R1bValue - sleepDriveSlope * cos(Psi))
}

R1b <- function(A, R1tot, Psi, params){
    r1totSqrtTerm = sqrt(pow(A + R1tot + 4.0, 2.0) - 4 * A * R1tot)
    return(0.5 * (A + R1tot + 4 - r1totSqrtTerm))
}

phaseResponse <- function(R, Psi, params) {
    A1 <- params[["A_1"]]
    A2 <- params[["A_2"]]
    Sigma <- params[["sigma"]]
    BetaL1 <- params[["beta_L1"]]
    BetaL2 <- params[["beta_L2"]]

    firstTerm <- A1 * 0.5 * (pow(R, 3) + 1.0 / R) * sin(Psi + BetaL1)
    secondTerm <- A2 * 0.5 * (1.0 + pow(R, 8)) + sin(2.0 * Psi + BetaL2)
    return(Sigma - firstTerm - secondTerm)
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

isStudentAwake <- function(t, R1bValue, Psi, inSchool, params){
	sleepThreshold <- params[["sleepThreshold"]]
	socialFactor <- params[["socialFactor"]]
    # Determine if student is awake
    if (inSchool) {
    	isAwake <- TRUE
    } else {
		# If it's free time, and you want to fall asleep, you can,
		# as long as you're not up for social reasons
		wantsToFallAsleep <- sleepDrive(R1bValue, Psi, params) > sleepThreshold
		afterSocialFactor <- (t %% 24) > socialFactor
		if (wantsToFallAsleep && afterSocialFactor) {
			isAwake <- FALSE
		} else {
			isAwake <- TRUE
		}
	}
	return(isAwake)
}

lightExposure <- function(t, isAwake, inSchool, params) {
	slope <- params[["lightSlope"]]
	riseTime <- params[["lightRiseTime"]]
	fallTime <- params[["lightFallTime"]]
	amplitude <- params[["lightAmplitude"]]
	baseline <- params[["lightBaseline"]]
	schoolBrightness <- params[["schoolBrightness"]]

	if (isAwake && inSchool) {
		light <- schoolBrightness
	} else {
		rise <- tanh(slope * ((t %% 24) - riseTime))
		fall <- tanh(slope * ((t %% 24) - fallTime))
		smoothLight <- (amplitude / 2.0) * (rise - fall) + baseline
		
		eyesClosed <- !isAwake
		light <- (1 - eyesClosed) * smoothLight
	}
    return(light)
}

sleepState <- function(t, R1bValue, Psi, inSchool, params) {
    # Returns mu, ichi, and light based on the current state of the model
    # 1. Determine if student isAwake using the current state information
    isAwake <- isStudentAwake(t, R1bValue, Psi, inSchool, params)
    # 2. Determine what light the student is exposed to
    light <- lightExposure(t, isAwake, inSchool, params)
    # 3. Select mu and ichi based on wake status
    if (isAwake) {
      ichi <- 1.0 / params[["awakeChi"]]
      mu <- params[["awakeMu"]]
    } else {
      ichi <-  1.0 / params[["asleepChi"]]
      mu <-  params[["asleepMu"]]
    } 
    return(c(mu, ichi, light))
}

studentInSchool <- function(t, params){
	schoolStart <- params[["schoolStart"]]
	schoolDuration <- params[["schoolDuration"]]
	gteSchoolStart <- (t %% 24 >= schoolStart)
	schoolEnd <- schoolStart + schoolDuration
	lteSchoolEnd <- (t %% 24 < schoolEnd)
	inSchoolHours <- gteSchoolStart & lteSchoolEnd
	isSchoolDay <- ((t %/% 24) %% 7) < 5
	inSchool <- isSchoolDay & inSchoolHours
	return(inSchool)
}

circadianModel <- function(t, x, params){
    # State variables
    R <- x[1]
    Psi <- x[2]
    n <- x[3]
    A <- x[4]
    R1tot <- x[5]
    # Get R1b from R1tot and A
    R1bValue <- R1b(A, R1tot, Psi, params)
    # Determine if student is in school hours and if it is a school day
	inSchool <- studentInSchool(t, params)
    # Determine if student is awake, and set mu, ichi, and light
    currentState <- sleepState(t, R1bValue, Psi, inSchool, params)
    mu <- currentState[1]
    ichi <- currentState[2]
    light <- currentState[3]
    # Calculate the right hand side of the ODEs
    K <- params[["K"]]
    Beta1 <- params[["beta_1"]]
    Tau <- params[["tau"]]
    Delta <- params[["delta"]]
	Gamma <- params[["gamma"]]
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
    dR1tot <- Irecep * (R1bValue - Targc * R1tot)
    # Return the right hand side of the ODEs
    dxdt <- c(dR, dPsi, dN, dA, dR1tot)
    # Return results as a list
    return(list(dxdt))
}
