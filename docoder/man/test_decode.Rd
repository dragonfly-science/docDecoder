\name{test_decode}
\alias{test_decode}
\title{
Compare output from decode_file() to CSV file with decoded data
}
\description{
Compare output from decode_file() to CSV file with decoded data
}
\usage{
test_decode(file)
}
\arguments{
  \item{file}{
Raw data file in extdata directory
}
}
\value{
If the output from `decode_file()` is the same as in the CSV with decoded
data, a message "Decoded data match those in csv file" is shown,
otherwise, prints the list of non-matching records.
}
\author{
Yvan Richard (Dragonfly Data Science - yvan@dragonfly.co.nz)
}

\examples{
encfile <- system.file('extdata', 'Inspection0000079628.txt', package='docoder')
test_decode(encfile)
}
