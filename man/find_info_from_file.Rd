\name{find_info_from_file}
\alias{find_info_from_file}

\title{
find_info_from_file
}
\description{
Find a particular information from a text file
}
\usage{
find_info_from_file(path,pattern,n=100)
}

\arguments{
  \item{path}{
 Text in which to search for the regular expression pattern
}
  \item{pattern}{
 Pattern allowing to retrieve a particular information in the text. The pattern needs to have a named capturing parenthesis. An example would be \code{"Damping factor = (?<capture>[0-9]*\\.?[0-9]*)"}, where the capturing name \code{<capture>} is mandatory.
}

\item{n}{
Number of lines to read while searching for the information. Provide \code{n=-1} for reading the entire file
}
}
\author{
Thomas Braschler
}

\keyword{ misc }
