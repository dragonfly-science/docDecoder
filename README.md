docoder
==========

R package to decode DOC visitor count data.

Installation
------------

To install in R:
```r
## install.packages('devtools')
library(devtools)
install_github('dragonfly-science/docoder', subdir='docoder')
```

Usage
-----

The main function, decode_string(), takes a string of encoded
data, and outputs a data frame with the date/time of the count and the count. 
The utility function decode_file() reads encoded data from a file, and passes it to
decode_string().

Examples
--------

```r
## Decode a string
enc.string <- "-127,-58,6,76,0,2,110,1,111,1,112,2,113,2,-127,-58,7,76,0,1,110,1,111,1"
decode_string(enc.string)

## Decode a file
cat("-127,-58,6,76,0,2,110,1,111,1,112,2,113,2,-127,-58,7,76,0,1,110,1,111,1", file='test.txt')
decode_file('test.txt')
```
