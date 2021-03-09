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
encoded in separate columns}

\author{
Thomas Braschler
}

\keyword{ misc }
