\name{find_infos_from_file}
\alias{find_infos_from_file}

\title{
find_infos_from_file
}
\description{
Searches a text file for the information elements specified by a vector of specific regular expression patterns
}
\usage{
find_infos_from_file(path,patterns)
                            
}

\arguments{
  \item{path}{
Path to the text file
}
  \item{patterns}{
Patterns allowing to retrieve the desired bits of information in the text file. The \code{patterns} needs are a vector, each element must have a named capturing parenthesis. An example would be \code{c("Damping factor = (?<capture>[0-9]*\\.?[0-9]*)","cut_lines=(?<capture>[0-9]*)")}, where the capturing name \code{<capture>} is mandatory.
}
}

\author{
Thomas Braschler
}


\keyword{ misc }

