## Test the simulateCircadianModel() function
timeArray <- seq(0, 72, length.out = 1000)
initialCondition <- c(R = 0.8, Psi = 2.5, n = 0.8)
lightArray <- ifelse(timeArray %% 24 >= 8 & timeArray %% 24 <= 22, 1000, 0)
result <- simulateCircadianModel(timeArray, lightArray, initialCondition,
                                 defaultParameters)
data <- data.frame(result)
# TODO: Test that period converges
# TODO: Test simulation in the darkness

## Test: Check that the model entrains to a 24h schedule
# TODO: Write tests

## Test the simulateSleepModel() function
timeArray <- seq(0, 72, length.out = 1000)
initialCondition <- c(A = 767.7, R1tot = 584.2)
wakeArray <- ifelse(timeArray %% 24 >= 8 & timeArray %% 24 <= 22, 1, 0)
result <- simulateSleepModel(timeArray, wakeArray, initialCondition,
                             defaultParameters)
data <- data.frame(result)
# TODO: Tests that when the wakeArray is 1,
#       variable A has a positive slope and when it is 0,
#       A has a negative slope

## Test: Sleep drive increases when awake and decreases when asleep
# TODO: Write tests


## Test the eventfunc() function
# TODO: Write tests

## Test the rootfunc() function
# TODO: Write tests

## Test the simulateTwoProcessModel() function
# TODO: Write tests
timeArray <- seq(0, 144, length.out = 5000)
initialCondition <- c(R = 0.8, Psi = 2.5, n = 0.8,
                      A = 760, R1tot = 580, S = 0)
lightArray <- ifelse(timeArray %% 24 >= 8 & timeArray %% 24 <= 22, 1000, 0)
result <- simulateTwoProcessModel(timeArray, lightArray, initialCondition,
                                  defaultParameters)
# data <- data.frame(result)
# R1b <- getR1b(data$A, data$R1tot)
# drive <- sleepDrive(R1b, data$Psi, params)
# plot(timeArray, drive, type = "l")

# attributes(result)$troot

## Test: Two-process model entrains to a 24h regular schedule when there is
##       no school at all
# TODO: Write tests

## Test: Two-process model switches correctly when there is school. This means
##       switching at thresholds but also switching up when the student should
##       be in school.
# TODO: Write tests
