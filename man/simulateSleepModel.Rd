% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{simulateSleepModel}
\alias{simulateSleepModel}
\title{simulateSleepModel Function}
\usage{
simulateSleepModel(timeArray, wakeArray, initialCondition, params)
}
\arguments{
\item{timeArray}{A numeric vector representing the time points for simulation.}

\item{wakeArray}{A numeric vector representing the wake values corresponding to the time points.}

\item{initialCondition}{A numeric vector representing the initial conditions for the sleep model.}

\item{params}{A list of parameters for the sleep model.}
}
\value{
A list containing the output of the LSODE solver.
}
\description{
This function simulates a sleep model using the LSODE solver.
}
