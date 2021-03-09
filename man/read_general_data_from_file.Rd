\name{read_general_data_from_file}
\alias{read_general_data_from_file}

\title{
read_general_data_from_file
}
\description{
Reads the general text data from the simulation output data files. This is up to, but without, the stress tensor data.
}
\usage{
read_general_data_from_file(path,stress_tensor_data_start_line=-1)
}

\arguments{
  \item{path}{
The path to the file to be read
}
  \item{stress_tensor_data_start_line}{
The line where the stress tensor data (including the header, but not the title) starts
}
}

\author{
Thomas Braschler
}

\keyword{ misc }

\examples{
general_data=read_general_data_from_file(system.file("full_python_simulation_output_example.txt",package="particleShearEvaluation"))
general_data[71:78]

}

