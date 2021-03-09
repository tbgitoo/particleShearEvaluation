\name{read_stress_tensor_data_from_file}
\alias{read_stress_tensor_data_from_file}

\title{
read_stress_tensor_data_from_file
}
\description{
Reads the stress tensor data in the file located at \code{path}
}
\usage{
read_stress_tensor_data_from_file(path,data_start_line=-1)}

\arguments{
  \item{path}{
 Path to the text file
}
  \item{data_start_line}{
 If known, provide the first line to read (the column headers; line numbering is assumed to start with 0 in the file); otherwise, provide -1 and an attempt is made to find the first line to read via \link{stress_data_start_line} with default arguments} 
}
\details{This function returns a plain dataframe, where the components of the various stress tensor are 
encoded in separate columns. For their meaning, see the particleShear python package.}

\author{
Thomas Braschler
}

\keyword{ misc }

\value{\link{data.frame} with the same columns as the ones configured in the text file, stress tneosr section}
	


\examples{
path=system.file("full_python_simulation_output_example.txt",package="particleShearEvaluation")
stress_tensor_data=read_stress_tensor_data_from_file(path)
# Some of the stress tensors are symmetrical, some not

# The classical Love-Weber tensors are not:
stress_tensor_asymmetry_Love_Weber=stress_tensor_data$stress_tensor_Love_Weber_01-stress_tensor_data$stress_tensor_Love_Weber_10
plot(stress_tensor_data$t,stress_tensor_asymmetry_Love_Weber,type="l", ylim=c(-20,20),main="Asymmetry: Love-Weber",ylab="Stress tensor asymmetry [Pa]")

dev.new()
# Otsuki et al. indicate a dynamic symmetry correction; in our experience, this does not correct the problem. The Otsuki correction is indeed orders of magnitude smaller than
# the asymmetry in the Love-Weber tensor to correct
stress_tensor_asymmetry_Otsuki=stress_tensor_data$stress_tensor_peculiar_acceleration_otsuki_01-stress_tensor_data$stress_tensor_peculiar_acceleration_otsuki_10
plot(stress_tensor_data$t,stress_tensor_asymmetry_Otsuki,type="l", ylim=c(-20,20),main="Asymmetry: Otsuki correction term",ylab="Stress tensor asymmetry [Pa]")
lines(stress_tensor_data$t,stress_tensor_asymmetry_Love_Weber,type="l", col="red")

dev.new()
# Stress tensors calculated uniquely by the externally visible forces are also asymmetric
stress_tensor_asymmetry_external=stress_tensor_data$stress_tensor_from_external_forces_01-stress_tensor_data$stress_tensor_from_external_forces_10
plot(stress_tensor_data$t,stress_tensor_asymmetry_external,type="l", ylim=c(-20,20),main="Asymmetry: Stress tensor from surface forces",ylab="Stress tensor asymmetry [Pa]")

dev.new()
# To symmetrize the stress tensors (Nicot et. al), one can use internal acceleration. Here, accounting done as for the external forces
stress_tensor_asymmetry_linear_acceleration=stress_tensor_data$stress_tensor_linear_acceleration_01-stress_tensor_data$stress_tensor_linear_acceleration_10
plot(stress_tensor_data$t,stress_tensor_asymmetry_linear_acceleration,type="l", ylim=c(-20,20),main="Asymmetry: Linear acceleration tensor",ylab="Stress tensor asymmetry [Pa]")
lines(stress_tensor_data$t,stress_tensor_asymmetry_external,type="l",col="red")

dev.new()
# To symmetrize the stress tensors (Nicot et. al), one can use acceleration stress tensors. The importance is matching: Here, accounting done as for the external forces
stress_tensor_asymmetry_linear_acceleration=stress_tensor_data$stress_tensor_linear_acceleration_01-stress_tensor_data$stress_tensor_linear_acceleration_10
plot(stress_tensor_data$t,stress_tensor_asymmetry_linear_acceleration,type="l", ylim=c(-20,20),main="Asymmetry: External linear acceleration tensor",ylab="Stress tensor asymmetry [Pa]")
lines(stress_tensor_data$t,stress_tensor_asymmetry_external,type="l",col="red")
legend("topleft",legend=c("linear acceleration correction of total system","external force tensor"),lty=c(1,1),col=c("black","red"))

dev.new()
# To symmetrize the stress tensors (Nicot et. al), one can use internal acceleration. As above, but accounting as for Love-Weber:
stress_tensor_asymmetry_internal_torques=stress_tensor_data$stress_tensor_internal_tangential_torque_01-stress_tensor_data$stress_tensor_internal_tangential_torque_10
plot(stress_tensor_data$t,stress_tensor_asymmetry_internal_torques,type="l", ylim=c(-20,20),main="Asymmetry: Internal torque correction",ylab="Stress tensor asymmetry [Pa]")
lines(stress_tensor_data$t,stress_tensor_asymmetry_Love_Weber,type="l",col="red")
legend("topleft",legend=c("internal torque correction","Love-Weber"),lty=c(1,1),col=c("black","red"))


# The relevant shear stress for evaluation is the column stress_tensor_data$shear_stress_internal_stress.tensor
# This is also  -(stress_tensor_data$stress_tensor_internal_tangential_torque_01+stress_tensor_data$stress_tensor_Love_Weber_01), or equivalently 
# -(stress_tensor_data$stress_tensor_internal_tangential_torque_10+stress_tensor_data$stress_tensor_Love_Weber_10) 
plot(stress_tensor_data$t,stress_tensor_data$shear_stress_internal_stress.tensor,type="l",ylab="Shear stress [Pa]", xlab="Time[s]")


}

\references{Love, A. E. H. A Treatise on the Mathematical Theory of Elasticity.  (Cambridge University Press, 1927).\cr   
Weber, J. Recherches concernant les contraintes intergranulaires dans les milieux pulvérulents. Bull. Liaison P. et Ch.20, 1-20 (1966).\cr
Otsuki, M. & Hayakawa, H. Discontinuous change of shear modulus for frictional jammed granular materials. Phys Rev E95, 062902 \cr
Nicot, F., Hadda, N., Guessasma, M., Fortin, J. & Millet, O. On the definition of the stress tensor in granular media. Int J Solids Struct50, 2508-2517, doi:10.1016/j.ijsolstr.2013.04.001 (2013).
}
