# Here we will have all the functions related to the circadian model
# We will be implementing Hannay's 2019 single population model plus
# the sleep drive model, just like you have in your code

# Model parameters
# TODO: add parameters
# alpha0 <- ...
# p <- ...
# I0 <- ...
# ...
# Define parameters vector
# parameters <- c(...)

# Model initial conditions
# TODO: add initial conditions
# ...
# initial_conditions <- c(R=..., Psi=..., n=..., A=..., R1tot=...)

# Model functions
bHat <- function(n, light){
    # TODO
    warning("Bhat not implemented")
}

alpha <- function(light){
    # TODO
    warning("alpha not implemented")
}

phaseResponse <- function(R, Psi){
    # TODO: Output the right hand side of dPsi/dt = ...
    warning("phaseResponse not implemented")
}

amplitudeResponse <- function(R, Psi){
    # TODO: Output the right hand side of dR/dt = ...
    warning("amplitudeResponse not implemented")
}

sleepDrive <- function(R1b, Psi){
    # TODO: Output the right hand side of dR1b/dt = ...
    warning("sleepDrive not implemented")
}

circadianModel <- function(t, x, params){
    # State variables
    R <- x[1]
    Psi <- x[2]
    n <- x[3]
    A <- x[4]
    R1tot <- x[5]

    # Get R1b from R1tot and A
    # TODO: R1b <- ...

    # Determine if student is in school hours and if it is a school day
    # TODO: inSchoolHours <- ...
    # TODO: isSchoolDay <- ...

    # Calculate the light intensity
    # TODO: light <- ...

    # Determine if student is awake
    # TODO: isAwake <- ...

    # Set mu and ichi parameters based on whether student is awake or not
    # TODO: mu <- ...
    # TODO: ichi <- ...

    # Store lux value for plotting later
    # TODO: light_schedule <- ...

    # Calculate the right hand side of the ODEs

    # Circadian model    
    # dR = ...
    # dPsi = ...
    # dN = ...    

    # Sleep drive model
    # dA = ...
    # dR1tot = ...

    # Return the right hand side of the ODEs
    # dxdt <- c(dR, dPsi, dN, dA, dR1tot)

    # Return results as a list
    # list(dxdt)
    warning("circadianModel not implemented")
}

