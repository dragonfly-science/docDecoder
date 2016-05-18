\name{decode_string}
\alias{decode_string}
\title{
Decode coded DOC visitor count data contained in a text string.
}
\description{
  This function takes an input string representing an input stream of
  comma-separated integers and decodes it, producing a data frame with the date/time of
  the count in POSIXct format and the number of visitors.
}
\usage{
decode_string(string, debug = FALSE, test = FALSE, summary = TRUE)
}
\arguments{
  \item{string}{
    path to the string containing the encoded data on visitor counts
  }
  \item{debug}{
    whether or not the intermediate data are retained in output data frame
  }
  \item{test}{
    test for the presence of NAs in `datetime` and `count` fields
  }
  \item{summary}{
    whether or not a short summary is printed (number of counts, range of
    datetimes, and range of counts)
  }
}
\value{
  A data frame with the columns `datetime` and `count`, if `debug` is
  FALSE.
  \item{datetime }{Date and time in POSIXct format of the count}
  \item{count }{The number of visitors counted}
  If `debug` is TRUE, the output data frame contains additional columns
  created in the process.
}
\author{
  Yvan Richard (Dragonfly Data Science - yvan@dragonfly.co.nz)
}

\examples{
enc.string <- "-127,-58,6,76,0,2,110,1,111,1,112,2,113,2,-127,-58,7,76,0,1,110,1,111,1"
d <- decode_string(enc.string)
d
}
\keyword{ manip }

