\name{read_stress_tensor_data_from_rda_file}
\alias{read_stress_tensor_data_from_rda_file}

\title{
read_stress_tensor_data_from_rda_file
}
\description{
Reads the stress tensor data in the file located at \code{path}
}
\usage{
read_stress_tensor_data_from_rda_file(path)

\arguments{
  \item{path}{
 Path to the rda file
}
}
\details{This merely reads the rda file specified and returns the \code{stress_data} variable which needs to be contained in the r data file specified by \code{path}}

\author{
Thomas Braschler
}

\keyword{ misc }

\value{The \code{stress_data} variable read from the .rda file indicated by \code{path}.}
	
\seealso{\link{read_stress_tensor_data_from_file} for reading the data directly from the large, raw text files; and \link{split_tensor_and_overview_data} for splitting the large direct output of the particleShear Python library into a header and rda file to save space}

\examples{
path=system.file("split/stress_full_python_simulation_output_example.rda",package="particleShearEvaluation")
stress_tensor_data=read_stress_tensor_data_from_rda_file(path)

#This should be the same variable as the one read with read_stress_tensor_data_from_file, see that example for further plots
}

\references{Love, A. E. H. A Treatise on the Mathematical Theory of Elasticity.  (Cambridge University Press, 1927).\cr   
Weber, J. Recherches concernant les contraintes intergranulaires dans les milieux pulvérulents. Bull. Liaison P. et Ch.20, 1-20 (1966).\cr
Otsuki, M. & Hayakawa, H. Discontinuous change of shear modulus for frictional jammed granular materials. Phys Rev E95, 062902 \cr
Nicot, F., Hadda, N., Guessasma, M., Fortin, J. & Millet, O. On the definition of the stress tensor in granular media. Int J Solids Struct50, 2508-2517, doi:10.1016/j.ijsolstr.2013.04.001 (2013).
}
