\name{file_information_table_from_folder}
\alias{file_information_table_from_folder}

\title{
Search a folder for text files to retrieve the standard header information from the particleShearSimulation output text files
}
\description{
Finds text files of at least \code{min_file_size} bytes in size, reads the first 100 lines (or all available lines if the file is shortr) for the header information, and looks for the standard information by means of regular expression patterns given by \link{headerSearchPatterns}
}
\usage{
file_information_table_from_folder(path,min_file_size=1e5)
}

\arguments{
  \item{path}{
  Folder path
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

