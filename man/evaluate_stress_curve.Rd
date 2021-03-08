\name{evaluate_stress_curve}
\alias{evaluate_stress_curve}

\title{
evaluate_stress_curve
}
\description{
Evaluate oscillatory sweep curve (G' and G'' as a function of amplitude), looking for a low strain plateau, a first softening transition, a soft plateau, and a yield transition
}
\usage{
evaluate_stress_curve(strain,Gprime,Gprimeprime,guess_softening_strain=0.01,guess_soft_plateau_strain=0.1)
}

\arguments{
  \item{strain}{
Strain, should be vector in ascending strain order
}
\item{Gprime}{Gprime values, same length as strain}


  \item{Gprimeprime}{
G'' values, should be the same length as previous arguments
}
  \item{guess_softening_strain}{
Initial guess for the softening strain, typically the same as the friction coefficient
}
\item{guess_soft_plateau_strain}{
Guess of the strain on the soft plateau, typically 0.1}

}

\value{
A matrix of 3 rows and 4 columns. First row is strain, second row is G', third row is G''. The columns are the low strain plateau, the softening transition, the soft plateau, and the yield transition}



\author{
Thomas Braschler
}

\keyword{ misc }

