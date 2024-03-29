% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/light.R
\name{lightExposure}
\alias{lightExposure}
\title{Calculate the student's light exposure for a given time}
\usage{
lightExposure(t, isAwake, inSchool, params)
}
\arguments{
\item{t}{Time in hours since the start of the week (0 = Sunday at midnight)}

\item{isAwake}{A Boolean indicating whether the student is awake}

\item{inSchool}{A Boolean indicating whether the student is in school}

\item{params}{A list of parameters. See below for details.}
}
\value{
The light exposure for the student as a scalar
}
\description{
`lightExposure()` calculates the light exposure for a student at a given time
taking into account the time of day, whether the student is awake, and
whether the student is in school.
}
\details{
Required parameters in `params` are:
- `lightSlope`: The slope of the light exposure curve (1/hours)
- `lightRiseTime`: The time of day when natural light exposure begins (hours)
- `lightFallTime`: The time of day when natural light exposure ends (hours)
- `lightAmplitude`: The amplitude of the light exposure curve (lux)
- `lightBaseline`: The baseline light exposure (lux)
- `schoolBrightness`: The light exposure in school (lux)
}
