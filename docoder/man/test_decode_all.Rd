\name{test_decode_all}
\alias{test_decode_all}
\title{
Tests decode_file() function with the all the example files.
}
\description{
Tests decode_file() function with the all the example files.
}
\usage{
test_decode_all()
}
\details{
Runs test_decode() for each of the example file.
}
\value{
  For each example file, whether the data decoded from decode_file()
  match the decoded data in the corresponding CSV file.
}
\author{
Yvan Richard (Dragonfly Data Science - yvan@dragonfly.co.nz)
}

\examples{
test_decode_all()
}
