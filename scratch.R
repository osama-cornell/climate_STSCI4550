library(devtools)
library(climate)
devtools:: load_all()
devtools::test()
devtools:: document()

#For time_series function
time_series(3047,"2003-05-22","2003-05-29")

#yearly cycle
yearly_cycle(3047)

