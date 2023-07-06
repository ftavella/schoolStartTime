# Try importing the circadian_model.R file and running a circadian simulation here

source(file.path("circadianRhythm.R"), local = TRUE)$value

#circadianRhythm(9,default_initial_conditions,params)



numberOfDays <- 49
dt<- 0.1
fullIntegrationWindow <- seq(from=0,to=numberOfDays * 24,by=dt)
  
isAwake <- TRUE
schoolStartLocalTimeInHours <- 6  # This is the start time for the school
schoolDurationInHours <- 7  # This is the duration of the school day
schoolBrightnessInLux <- 300  # This is the brightness of the school
  
allSchoolStartOptions <- c(5, 6, 7, 8, 9, 10)
sleepDurationSchool <- c()
   
    
for (schoolStartLocalTimeInHours in allSchoolStartOptions) {
    allLux <<- -1 + numeric(numberOfDays * 24 / dt + 1)  # Sanity check vector to make sure we're getting the right lux
    
    out <- rk4(x, fullIntegrationWindow, circadianModel, params)
    
    
    homeostatLastWeek <- tail(out[,4], 24 * 7 / dt)
    homeostatDiff <- diff(homeostatLastWeek)
   # cat(sprintf("Wake time for school: %f\n", schoolStartLocalTimeInHours))
    avgSleep <- (24 * length(homeostatDiff[homeostatDiff < 0])/length(homeostatDiff))
   # cat(sprintf("Average sleep on this schedule over a week: %f\n", avgSleep))
    
    homeostatLastWeekDiff <- head(homeostatDiff, 24 * 5 / dt)
    avgSleepSchool <- (24 * length(homeostatLastWeekDiff[homeostatLastWeekDiff < 0])/length(homeostatLastWeekDiff))
   # cat(sprintf("Average sleep during school schedule over a week: %f\n", avgSleepSchool))
    sleepDurationSchool <- append(sleepDurationSchool, avgSleepSchool)
    homeostatFirstWeek <- head(out[,4], 24 * 7 / dt)
    homeostatToPrint <- homeostatLastWeek

   # return(sleepDurationSchool)
 }

for (sleepy in sleepDurationSchool) {
   circadianRhythm(sleepy, default_initial_conditions, params)
   plot(sleepy, allLux)
   plot(sleepy, default_initial_conditions[1])
   plot(sleepy, default_initial_conditions[2])
   plot(sleepy, default_initial_conditions[3])
   plot(sleepy, default_inital_conditions[4])
   plot(sleepy, default_initial_conditions[5])
   

}

