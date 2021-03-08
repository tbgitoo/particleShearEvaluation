\name{read_stress_curve_from_file}
\alias{read_stress_curve_from_file}

\title{
read_stress_curve_from_file
}
\description{
Reads the primary stress curves stored in a text file located at \code{path}
}
\usage{
read_stress_curve_from_file(path,data_start_line=-1,time_column="t",strain_column="strain",
strain_rate_column="strain_rate",shear_stress_measured_at_surface_column="shear_stress_measured_at_surface",
shear_stress_from_internal_stress_tensor_column="shear_stress_from_internal_stress_tensor")}

\arguments{
  \item{path}{
 Path to the text file
}
  \item{data_start_line}{
 If known, provide the first line to read (the column headers; line numbering is assumed to start with 0 in the file); otherwise, provide -1 and an attempt is made to find the first line to read via \link{stress_data_start_line} with default arguments} 
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
