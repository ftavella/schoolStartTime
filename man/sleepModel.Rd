% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sleep.R
\name{sleepModel}
\alias{sleepModel}
\title{Calculates the rate of change of A and R1tot}
\usage{
sleepModel(t, x, S, params)
}
\arguments{
\item{t}{Time in hours since the start of the week (0 = Sunday at midnight)}

\item{x}{Current state}

\item{S}{The current wakefulness state. 1 for awake, 0 for asleep.}

\item{params}{A list of additional parameters including Irecep and Targc.}
}
\value{
A vector containing the rate of change of A and R1tot.
}
\description{
Calculates the rate of change of A and R1tot
}
