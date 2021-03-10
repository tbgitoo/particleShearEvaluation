\name{file_information_table_from_folders}
\alias{file_information_table_from_folders}

\title{
Search several folders for text files to retrieve the standard header information from the particleShearSimulation output text files
}
\description{
Finds text files of size of at least \code{min_file_size} bytes, reads the first 100 lines for the header information (or the entire file if less than 100 lines are found), and looks for the standard information by means of regular expression patterns given by \link{headerSearchPatterns}
}
\usage{
file_information_table_from_folders(paths,min_file_size=1e5)
}
\details{
For each element in \code{paths}, this function invokes \link{file_information_table_from_folders}, and then concatenates the results
}

\arguments{
  \item{paths}{
  Vector of folder paths
}
  \item{min_file_size}{
  Minimal size for a file to be taken into account in the listing
}

}
\value{
A data frame, one line per conforming file, columns for each information element
}
\author{
Thomas Braschler
}

\keyword{ misc }

