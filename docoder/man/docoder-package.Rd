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
The main function, decode_string(), takes a string of encoded
data, and outputs a data frame with the date/time of the count and the count. 
The utility function decode_file() reads encoded data from a file, and passes it to
decode_string().
}
\author{
Yvan Richard (Dragonfly Data Science - yvan@dragonfly.co.nz)

Maintainer: Yvan Richard <yvan@dragonfly.co.nz>
}
\examples{
## Decode a string
enc.string <- "-127,-58,6,76,0,2,110,1,111,1,112,2,113,2,-127,-58,7,76,0,1,110,1,111,1"
decode_string(enc.string)

## Decode a file
cat("-127,-58,6,76,0,2,110,1,111,1,112,2,113,2,-127,-58,7,76,0,1,110,1,111,1", file='test.txt')
decode_file('test.txt')

}
