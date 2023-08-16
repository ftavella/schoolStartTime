# Here we will have all the functions related to the circadian model
# We will be implementing Hannay's 2019 single population model plus
# the sleep drive model, just like you have in your code
library(deSolve)
library(tidyverse)

# Model parameters
Dwake <-555.4
Dsleep <- 572.7 
# TODO: add parameters
alpha0 <- 0.05
p <- 1.5
I0 <- 9325.0
Delta <- 0.0075
G <- 33.75

Irecep <- 1 / 291.0
Targc <- 0.9677
Tau <- 23.84
Beta1 <- -0.09318
A1    <- 0.3855
A2    <- 0.1977
BetaL1<- -0.0026 
BetaL2<- -0.957756
Sigma <- 0.0400692
K     <- 0.06458
Gamma <- 0.024

# Define parameters vector
sleepParameters <- c(Dwake, Dsleep)
lightParameters <- c(alpha0, p, I0, Delta, G)
circadianParameters <- c(Irecep, Targc, Tau, Beta1, A1, A2, BetaL1, BetaL2, Sigma, K, Gamma)

# Model initial conditions
# TODO: add initial conditions
# ...
default_initial_conditions <- c(R=0.8240484019863923, Psi=2.4532409247791804, n=0.78424752754260862, A=767.657, R1tot=584.24)

chiAwake <- 1.0/18.18
muAwake <- 869.5
chiAsleep <- 1.0/7.0
muAsleep <- 596.5

#params <- c(Dwake, Dsleep, alpha0, p, I0, Delta, G, Irecep, Targc, Tau, Beta1, A1, A2, BetaL1, BetaL2, Sigma, K, Gamma, chiAwake, muAwake, chiAsleep, muAsleep)

pow <- function(a, p) {
  a ** p 
}

# Model functions

alpha <- function(light){
    # TODO
    return (alpha0 * pow(light, P) / (pow(light, P) + I0))
  
}

bHat <- function(n, light){
    # TODO
    return (G * (1.0 - n) * alpha(light))
    
}

phaseResponse <- function(R, Psi){
    # TODO: Output the right hand side of dPsi/dt = ...
    firstTerm <- A1 * 0.5 * (pow(R, 3) + 1.0 / R) * sin(Psi + BetaL1)
    secondTerm <- A2 * 0.5 * (1.0 + pow(R, 8)) + sin(2.0 * Psi + BetaL2)
    return (Sigma - firstTerm - secondTerm)
    
}

amplitudeResponse <- function(R, Psi){
    # TODO: Output the right hand side of dR/dt = ...
    firstTerm <- A1 * 0.5 * (1.0 - pow(R, 4)) * cos(Psi + BetaL1)
    secondTerm <- A2 * 0.5 * R * (1.0 - pow(R, 8)) * cos(2.0 * Psi + BetaL2)
    return(firstTerm + secondTerm)
                                  
}

sleepDrive <- function(R1b, Psi){
    # TODO: Output the right hand side of dR1b/dt = ...
    return(R1b - 3.25 * cos(Psi))
    # R1b - 3.25 * 0.5 * (1 + 0.8*sin(Psi) - 0.47*cos(Psi))
    
}

circadianModel <- function(t, x, params){
    # State variables
    R <- x[1]
    Psi <- x[2]
    n <- x[3]
    A <- x[4]
    R1tot <- x[5]

    # Get R1b from R1tot and A
    R1b <- R1b <- 0.5 * (A + R1tot + 4 - sqrt(pow(A + R1tot + 4,2.0) - 4 * A * R1tot))

    # Determine if student is in school hours and if it is a school day
    inSchoolHours <- (t %% 24 > schoolStartLocalTimeInHours) && (t %% 24 <= schoolStartLocalTimeInHours + schoolDurationInHours)
    isSchoolDay <- ((t %/% 24) %% 7) < 5

    # Calculate the light intensity
    light <- lightApprox(t %% 24)$y

    # Determine if student is awake
    if (inSchoolHours && isASchoolDay) {
      isAwake <<- TRUE
      light <- schoolBrightnessInLux
    } else {
      
      # If it's free time, and you want to fall asleep, you can, as long as you're not up for social reasons
      if (sleepDrive(R1b, Psi) > Dsleep) {  
        if (t %% 24 > socialFactor || t %% 24 < 10) { # Forbid falling asleep before a certain time due to social reasons
          isAwake <<- FALSE
        }
      }
      # If it's free time, and you want to wake up, you can wake up
      if (sleepDrive(R1b, Psi) < Dwake) {   
        isAwake <<- TRUE
      }
    }

    # Set mu and ichi parameters based on whether student is awake or not
     
    if (isAwake) {
      ichi <- chiAwake
      mu <- muAwake
      if (light < baselineLight) { # Set light to baselineLight if they're awake and it's dark outside
        light <- baselineLight
      }
    } else {
      ichi <- chiAsleep  # Originally 1.0/4.2
      mu <- muAsleep
      light <- 0  # Set light to 0 if they're asleep
    }

    # Store lux value for plotting later
    if (allLux[1 + floor(t / dt)] == -1) {
      allLux[1 + floor(t / dt)] <<- light
    }

    # Calculate the right hand side of the ODEs

    alpha_L <- alphaL(light)
    BhatValue <- Bhat(N, light)
    
    # Circadian model    
    lightAmp <- BhatValue * amplitudeResponseCurve(R, Psi)
    lightPhase <- BhatValue * phaseResponseCurve(R, Psi)
    couplingTermAmp <- K * 0.5 * cos(Beta1) * R * (1.0 - pow(R,4))
    
    dR = (-1.0 * Gamma * R) + couplingTermAmp + lightAmp

    naturalOscillation <- 2.0 * pi / Tau
    couplingTermPhase <- K / 2.0 * sin(Beta1) * (1 + pow(R,4))
    dPsi <- naturalOscillation + couplingTermPhase + lightPhase
    
    dN <- 60.0 * (alpha_L * (1 - N) - Delta * N)   

    # Sleep drive model
    dA <- ichi * (mu - A)
    dR1tot <- Irecep * (R1b - Targc * R1tot)

    # Return the right hand side of the ODEs
    dxdt <- c(dR, dPsi, dN, dA, dR1tot)

    # Return results as a list
    return(list(dxdt))
    
   
     }
  
}

