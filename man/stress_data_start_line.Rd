\name{stress_data_start_line}
\alias{stress_data_start_line}

\title{
stress_data_start_line
}
\description{
Finds the line where the stress data starts in the file identified by \code{path}
}
\usage{
stress_data_start_line(path,data_header_pattern="Detailed force and stress data",n_header=200)
}

\arguments{
  \item{path}{
 Path to the text file
}
  \item{data_header_pattern}{
 Pattern identifying the line prior to data start (perl-style, see \link{grep}). }

\item{n_header}{
Number of lines to read while searching for the information. Provide \code{n_header=-1} for reading the entire file. This argument is passed to \link{read_first_lines}, used internally.
}
}
\details{The \code{pattern} argument should uniquely identify the data start line (if not possible, it should at least identify the first occurrence). Also, it should match the text line right above the first data line to read. If in addition to numerical data, there is a header line labelling the data, the pattern needs to identify a text line ONE LINE ABOVE the header line; alternatively, 1 needs to be subtracted from the line number returned}

\author{
Thomas Braschler
}

\keyword{ misc }
