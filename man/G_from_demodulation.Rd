\name{G_from_demodulation}
\alias{G_from_demodulation}

\title{
G_from_demodulation
}
\description{
G' and G'' by demodulation of a time - shear stress curve
}
\usage{
G_from_demodulation(t,Frequency,m,correct_for_jumps=TRUE,Strain_amplitude=1,relative_threshold=0.15,doPlot=FALSE,plot.new=TRUE,max_plot=400,...)
}

\arguments{
  \item{t}{
Vector with the time points
}
\item{Frequency}{Frequency of applied oscillatory shear}


  \item{m}{
Vector with the shear stress measurements, should have same length as \code{t}
}
  \item{correct_for_jumps}{
Possibility to correct for sudden jumps, arising typically as individual spheres transit across the boundary under Lees-Edwards conditions
}
\item{Strain_amplitude}{
Amplitude of the applied sinusoidal strain}
\item{relative_threshold}{
Threshold relative to minimum to maximum amplitude found in the measurement \code{m} }
\item{doPlot}{
Plot the jump-corrected curve? }
\item{plot.new}{Start a new plot? Only relevant, if \code{doPlot=TRUE}}
\item{max_plot}{Maximum number of values to plot for the stress curve. Only relevant if \code{doPlot=TRUE} and if \code{max_plot<length(t)}} 
\item{...}{Additional parameters to be passed to the graphical functions (either \link{plot} if 
\code{doPlot=TRUE} or otherwise \link{lines}}
}

\value{
Vector of two named elements (\code{"Gprime"} and \code{"Gprimeprime"}).}

\details{
The function assumes that the first time point is at \code{t=0}; if not the case, the initial time is subtracted from all time values.\cr Second, the in-phase component (the \code{"Gprime"} element in the output) is proportional to the sinus component, whereas the out-of-phase component (the \code{"Gprimeprime"} element in the output) is proportional to the cosine component}


\author{
Thomas Braschler
}

\keyword{ misc }

