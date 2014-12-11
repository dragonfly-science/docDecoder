all: pkg

pkg:
	R CMD check docoder &&  R CMD build docoder

install: 
	R CMD INSTALL docoder --byte-compile
