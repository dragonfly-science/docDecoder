\name{docoder-package}
\alias{docoder-package}
\alias{docoder}
\docType{package}
\title{
  Decode DOC visitor count data
}
\description{
Simple package providing the main function decode_file() to decode the
DOC data on visitor counts
}
\details{
\tabular{ll}{
Package: \tab docoder\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-12-11\cr
License: \tab GPL-3\cr
}
The main function, decode_file(), takes the path of a text file with the encoded
data, and outputs a data frame with the date/time of the count and the count.
}
\author{
Yvan Richard (Dragonfly Data Science - yvan@dragonfly.co.nz)

Maintainer: Yvan Richard <yvan@dragonfly.co.nz>
}
\examples{
## Creates a test file with encoded data
cat("-127,-58,6,76,0,2,110,1,111,1,112,2,113,2,-127,-58,7,76,0,1,110,1,111,1", file='test.txt')

d <- decode_file('test.txt')
d

}
