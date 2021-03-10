\name{read_stress_curve_from_rda_file}
\alias{read_stress_curve_from_rda_file}

\title{
read_stress_curve_from_rda_file
}
\description{
Reads the primary stress curves stored in a rda file located at \code{path}
}
\usage{
read_stress_curve_from_rda_file(path,time_column="t",strain_column="strain",
strain_rate_column="strain_rate",shear_stress_measured_at_surface_column="stress_measured_at_surface",
shear_stress_from_internal_stress_tensor_column="shear_stress_internal_stress.tensor")
}

\arguments{
  \item{path}{
 Path to the rda
}
\item{time_column}{
Header of the time column; by default, this is \code{"t"}
}
\item{strain_column}{
Header of the strain column; by default, this is \code{"strain"}
}
\item{strain_rate_column}{
Header of the strain rate column; by default, this is \code{"strain_rate"}
}
\item{shear_stress_measured_at_surface_column}{
Header of the stress column, as measured by force per unit of surface area at the boundaries where 
shear force is applied. The stress as measured at the surface most closely emulates what will be measured in a rheometer}
\item{shear_stress_from_internal_stress_tensor_column}{
Header of the interal stress column; the internal stress is a volume-averaged shear stress corrected for 
inertial effects, see eq. 15 in Otsuki et al. }

}
\details{If inertial effects can be neglected, or if correction for them is perfect, the internal stress quantification (column \code{shear_stress_from_internal_stress_tensor_column}) should match the surface stress (column \code{shear_stress_measured_at_surface_column}). Large discrepancies between the two indicate that the simulation should is primarily dynamic with imperfect correction rather than quasistatic}

\author{
Thomas Braschler
}

\references{
Otsuki, M. and H. Hayakawa, Discontinuous change of shear modulus for frictional jammed granular materials. Phys Rev E, 2017. 95(6-1): p. 062902.
}

\keyword{ misc }

\examples{
# This is the same data stored in rda format that is also read from text for read_stress_curve_from_file
path=system.file("split/stress_full_python_simulation_output_example.rda",package="particleShearEvaluation")
stress_curve=read_stress_curve_from_rda_file(path)
plot(strain ~ t, stress_curve,type="l",xlab="t[s]",ylab="strain [-], stress[kPa]")
lines(stress_curve$t, stress_curve$shear_stress_from_internal_stress_tensor/1000,type="l",col="red")
legend("bottomleft",legend=c("strain","stress"),lty=c(1,1),col=c("black","red"))
}
