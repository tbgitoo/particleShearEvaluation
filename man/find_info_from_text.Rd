\name{find_info_from_text}
\alias{find_info_from_text}

\title{
find_info_from_text
}
\description{
Find particular information from the file header text by using a supplied regular expression pattern
}
\usage{
find_info_from_text(txt,pattern)
}

\arguments{
  \item{txt}{
 Text in which to search for the regular expression pattern
}
  \item{pattern}{
 Pattern allowing to retrieve a particular information in the text. The pattern needs to have a named capturing parenthesis. An example would be \code{"Damping factor = (?<capture>[0-9]*\\.?[0-9]*)"}, where the capturing name \code{<capture>} is mandatory.
}
}

\author{
Thomas Braschler
}

\keyword{ misc }
