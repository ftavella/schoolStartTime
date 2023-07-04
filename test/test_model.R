# Try importing the circadian_model.R file and running a circadian simulation here

source(file.path("circadianRhythm.R"), local = TRUE)$value

circadianRhythm(9,initial_conditions,params)
