library(devtools)
library(climate)
devtools:: load_all()
devtools::test()
devtools:: document()

#For time_series function
time_series(3047,"2003-05-22","2003-05-29")

#yearly cycle
yearly_cycle(3047)

#yearly trend
yearly_trend(3047,"T_DAILY_AVG")

#grid points
grid_points(2)

#interpolations
station_grid_points(grid_points(2))

#plots
plot_interpolations(station_grid_points(grid_points(2)))
