library(lineprof)

source('source.r')

prof <- lineprof(run())

shine(prof)
