\name{decode_file}
\alias{decode_file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Decode coded DOC visitor count data contained in a text file.
}
\description{
  This function takes an input file containing an input stream of
  integers and decodes it, producing a data frame with the date/time of
  the count in POSIXlt format and the number of visitors.
}
\usage{
decode_file(file, debug = FALSE, test = TRUE)
}
\arguments{
  \item{file}{
path to the text file containing the encoded data on visitor counts
}
  \item{debug}{
whether or not the intermediate data are retained in output data frame
}
  \item{test}{
test for the presence of NAs in 'datetime' and 'count' fields
}
}
\value{
A data frame with the columns 'datetime' and 'count', if debug is
FALSE.
\item{datetime }{Date and time in POSIXlt format of the count}
\item{count }{The number of visitors counted}
}
\author{
Yvan Richard (Dragonfly Data Science - yvan@dragonfly.co.nz)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
cat("-127,-58,6,76,0,2,110,1,111,1,112,2,113,2,-127,-58,7,76,0,1,110,1,111,1", file='test.txt')
d <- decode_file('test.txt')
d
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

