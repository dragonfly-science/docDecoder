\name{test_decode}
\alias{test_decode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compare output from decode_file() to CSV file with decoded data
}
\description{
Compare output from decode_file() to CSV file with decoded data
}
\usage{
test_decode(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
file in test_data directory
}
}
\value{
If the output from decode_file() is the same as in the CSV with decoded
data, a message "Decoded data match those in csv file" is shown,
otherwise, returns the list of non-matching records.
}
\author{
Yvan Richard (Dragonfly Data Science - yvan@dragonfly.co.nz)
}

\examples{
encfile <- system.file('extdata', 'Inspection0000079628.txt', package='docoder')
test_decode(encfile)
}
