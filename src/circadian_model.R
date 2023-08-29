library(deSolve)
library(tidyVerse)
source(file.path(“test/test_model.R”), local = TRUE)$value 


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

smooth_light <- function(t) {

           rise <- tanh(0.6 * ((t %%24) - 8.0))

           fall <- tanh(0.6 * ((t %%24)- 17))

           return ((700.0/2.0) * (rise - fall) + 40.0)

}

sleepDrive <- function(R1b, Psi, params){
    sleepDriveSlope <- params[["sleepDriveSlope"]]
    return(R1b - sleepDriveSlope * cos(Psi))
}

typicalLight <- function(t, params){
    #TODO: create a function that returns 
    # the typical light intensity in a day
    # at a given time
    # t will come in as t %% 24
    light <- 0
    
    schoolInfo <- c((t %% 24 > params[["schoolStart"]]) && (t %% 24 <= (params[["schoolStart"]] + params[["schoolDuration"]])), ((t %/% 24) %% 7) < 5)

    #schedule <- LightSchedule(smooth_light, period = 24)
    #time_ar <- seq(from = 8.0, to = 24 * numberOfDays + 8.0, by = dt) 

    if (schoolInfo[1] && schoolInfo[2]) {
         awake <<- TRUE
        
    } else {

    
    # If it's free time, and you want to fall asleep, you can, as long as you're not up for social reasons
       if (sleepDrive(R1b, Psi, params) > params[["sleepThreshold"]]) {  
          # set socialFactor = 14
        if (t %% 24 > params[["socialFactor"]] || t %% 24 < 10) { # Forbid falling asleep before a certain time due to social reasons
          awake <<- FALSE
        }
       }
       # If it's free time, and you want to wake up, you can wake up
       if (sleepDrive(R1b, Psi, params) < params[["wakeThreshold"]]) {   
        awake <<- TRUE
       } 

    }
    
    if (awake == TRUE) {
        s <<- 0 
      } else {
        s <<- 1
    }    

   
    
    # I = (1 - S)I(t)
    # S = 0 if awake & S = 1 if asleep
    light = (1 - s) * smooth_light(t)
    

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

    schoolInfo <- c((t %% 24 > params[["schoolStart"]]) && (t %% 24 <= (params[["schoolStart"]] + params[["schoolDuration"]])), ((t %/% 24) %% 7) < 5)
    
    # Get R1b from R1tot and A
    R1b <- 0.5 * (A + R1tot + 4 - sqrt(pow(A + R1tot + 4,2.0) - 4 * A * R1tot))

    # Determine if student is in school hours and if it is a school day
    # Note: Letting schoolStartLocalTimeinHours = 8 for pragmaticism; subbing this in:
    inSchoolHours <- schoolInfo[1] # 8 + 7 = 15 (schoolStartLocalTimeinHours + duration)
    isSchoolDay <- schoolInfo[2]

  

    # Determine if student is awake
    if (inSchoolHours && isASchoolDay) {
      isAwake <<- TRUE
      light <- params[["schoolBrightness"]]
    } else {
      
      # If it's free time, and you want to fall asleep, you can, as long as you're not up for social reasons
      if (sleepDrive(R1b, Psi, params) > params[["sleepThreshold"]]) {  
          # set socialFactor = 14
        if (t %% 24 > params[["socialFactor"]] || t %% 24 < 10) { # Forbid falling asleep before a certain time due to social reasons
          isAwake <<- FALSE
        }
      }
      # If it's free time, and you want to wake up, you can wake up
      if (sleepDrive(R1b, Psi, params) < params[["wakeThreshold"]]) {   
        isAwake <<- TRUE
      }
    }

    # Set mu and ichi parameters based on whether student is awake or not
     
    if (isAwake) {
      ichi <- 1.0/18.18
      mu <- 869.5
      if (light < params[["baselineLight"]]) { # Set light to baselineLight if they're awake and it's dark outside
        light <- params[["baselineLight"]]
      }
    } else {
      ichi <- 1.0/7.0  # Originally 1.0/4.2
      mu <- 596.5
      light <- 0  # Set light to 0 if they're asleep
    }

    # Store lux value for plotting later
#   if (allLux[1 + floor(t / dt)] == -1) {
 #     allLux[1 + floor(t / dt)] <<- light
  #  }
    
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
