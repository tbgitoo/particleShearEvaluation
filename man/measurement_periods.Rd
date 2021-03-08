\name{measurement_periods}
\alias{measurement_periods}

\title{
measurement_periods
}
\description{
Identification of different phases of the oscillatory shear experiment
}
\usage{
measurement_periods(shear_stress_data,baseline_pre_periods,periods,Frequency)
}

\arguments{
  \item{shear_stress_data}{
A shear stress data frame. Needs to have at least a column named \code{"t"}
}
  \item{baseline_pre_periods}{
The number of periods of pre-equilibration before applying shear
}
  \item{periods}{
The number of periods of application of shear
}
\item{Frequency}{Frequency of applied oscillatory shear}
}

\value{
Vector of as many elements as there are rows in \code{shear_stress_data}. This function returns a factor with four levels: \code{"pre"}, \code{"initiation"}, \code{"oscillatory_measurement"}, and \code{"post"}. Entries with \code{"pre"} signify that the time-point is before initiation of the oscillatory shear; \code{"initiation"} is initiation (first period if \code{periods>1}); \code{"oscillatory_measurement"} indicates the appropriate measurement period; \code{"post"} indicates re-equilibration after cessation of shear}



\author{
Thomas Braschler
}

\keyword{ misc }

