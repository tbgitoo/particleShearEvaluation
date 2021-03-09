\name{get_yield_point}
\alias{get_yield_point}

\title{
get_yield_point
}
\description{
Yield point by crossing of G' and G'' curves in a strain vs. G' and G'' oscillatory sweep diagram
}
\usage{
get_yield_point(strain,Gprime,Gprimeprime)
}

\arguments{
  \item{strain}{
Vector with strain values (meant to be imposed in the simulations), should be unique values
}
\item{Gprime}{Vector with G' values, identical in length to \code{strain}}
\item{Gprimeprime}{Vector with G'' values, identical in length to \code{strain}}
}

\value{
Strain at which the G' and G'' curves intersect}

\details{
This is a relatively elementary function to find the intersection of the G' and G'' curves. It proceeds as follows: If G'' is higher than G' everywhere, the lowest strain value is reported; if on the contrary, G' is higher than G'' everywhere, \code{NA} is reported. In the nominal case, the highest strain at which G' crosses below G'' is reported.}

\author{
Thomas Braschler
}

\seealso{evaluateYield in package rheologyEvaluation (https://github.com/tbgitoo/rheologyEvaluation) with more flexible options for more complicated cases}

\keyword{ misc }

\examples{
strain=c(0.01,0.03,0.1,0.3,1,3,10)
Gprime=c(1000,950,700,300,100,20,10)
Gprimeprime=c(50,60,80,100,120,110,50)
plot(Gprime ~ strain,type="b",ylab=c("G' and G''"))
lines(Gprimeprime ~ strain)
lines(rep(get_yield_point(strain,Gprime,Gprimeprime),2),c(0,max(Gprime)),lty=2)
get_yield_point(strain,Gprime,Gprimeprime)
}

